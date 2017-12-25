#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_ty" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:52 2017

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
 This file contains the type mechanism with simple inheritance
*/
/*
*/
/*
 6/29/85: Original version written
*/
/*
 1/24/86: Added major types
*/
/*
 9/23/86: Rewrote to be flavorless
*/
/*
*/
/*
*******************************************************************************
*/
/*
(rtrace)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:451 **********************/
:-lisp_compile_to_prolog(pkg_user,[rtrace])
:- f_sys_rtrace(_Ignored4).
/*
(ob$decl-inverses 'isa 'isa-of)
;(ob$decl-inverses 'type 'type-of)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:462 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$decl-inverses',[quote,isa],[quote,'isa-of']])
:- f_u_ob_c36_decl_inverses(u_isa, u_isa_of, _Ignored4).
/*
(ob$decl-inverses 'type 'type-of)
*/
/*
(defun ty$instance? (self type-name)
  (if (not (ob? self))
      (progn (error "ty$instance?: "(defun ty$instance? (self type-name)\n  (if (not (ob? self))\n      (progn (error \"ty$instance?: ~A not ob\" self)\n             (ndbg-roman-nl *gate-dbg* ob-warn\n                            \"Warning: ty$instance?: ~A not ob\" self)\n             nil)\n      (and (ty? (ob$get self 'type))\n           (or (eq? type-name (ob$get self 'type))\n               (any? (lambda (x) (memq? type-name (ob$names x)))\n                     (ty$supertypes* (ob$get self 'type)))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:530 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$instance?',[self,'type-name'],[if,[not,['ob?',self]],[progn,[error,'$STRING'("ty$instance?: ~A not ob"),self],['ndbg-roman-nl','*gate-dbg*','ob-warn','$STRING'("Warning: ty$instance?: ~A not ob"),self],[]],[and,['ty?',['ob$get',self,[quote,type]]],[or,['eq?','type-name',['ob$get',self,[quote,type]]],['any?',[lambda,[x],['memq?','type-name',['ob$names',x]]],['ty$supertypes*',['ob$get',self,[quote,type]]]]]]]])
wl:lambda_def(defun, u_ty_c36_instance_c63, f_u_ty_c36_instance_c63, [u_self, u_type_name], [[if, [not, [u_ob_c63, u_self]], [progn, [error, '$ARRAY'([*], claz_base_character, "ty$instance?: ~A not ob"), u_self], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: ty$instance?: ~A not ob"), u_self], []], [and, [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]], [or, [u_eq_c63, u_type_name, [u_ob_c36_get, u_self, [quote, type]]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_type_name, [u_ob_c36_names, u_x]]], [u_ty_c36_supertypes_xx, [u_ob_c36_get, u_self, [quote, type]]]]]]]]).
wl:arglist_info(u_ty_c36_instance_c63, f_u_ty_c36_instance_c63, [u_self, u_type_name], arginfo{all:[u_self, u_type_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_type_name], opt:0, req:[u_self, u_type_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_instance_c63).

/*

### Compiled:  `U::TY$INSTANCE?` 
*/
f_u_ty_c36_instance_c63(Self, Type_name, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_self, Self), bv(u_type_name, Type_name)|Env],
	f_u_ob_c63(u_self, PredArgResult),
	(   PredArgResult==[]
	->  get_var(Env18, u_self, Self_Get),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"ty$instance?: ~A not ob"),
		       Self_Get
		     ],
		     Error_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_ob_warn,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Warning: ty$instance?: ~A not ob"),
				u_self
			      ],
			      Roman_nl_Ret),
	    FnResult=[]
	;   f_u_ty_c63([u_ob_c36_get, u_self, [quote, type]], IFTEST11),
	    (   IFTEST11\==[]
	    ->  (   f_u_eq_c63(u_type_name,
			       [u_ob_c36_get, u_self, [quote, type]],
			       FORM1_Res),
		    FORM1_Res\==[],
		    FnResult=FORM1_Res
		->  true
		;   f_u_any_c63(
				[ lambda,
				  [u_x],
				  
				  [ u_memq_c63,
				    u_type_name,
				    [u_ob_c36_names, u_x]
				  ]
				],
				
				[ u_ty_c36_supertypes_xx,
				  [u_ob_c36_get, u_self, [quote, type]]
				],
				Any_c63_Ret),
		    FnResult=Any_c63_Ret
		)
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_ty_c36_instance_c63, classof, claz_function),
   set_opv(u_ty_c36_instance_c63, compile_as, kw_function),
   set_opv(u_ty_c36_instance_c63, function, f_u_ty_c36_instance_c63),
   _Ignored4=u_ty_c36_instance_c63.
/*
:- side_effect(assert_lsp(u_ty_c36_instance_c63,
			  wl:lambda_def(defun, u_ty_c36_instance_c63, f_u_ty_c36_instance_c63, [u_self, u_type_name], [[if, [not, [u_ob_c63, u_self]], [progn, [error, '$ARRAY'([*], claz_base_character, "ty$instance?: ~A not ob"), u_self], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: ty$instance?: ~A not ob"), u_self], []], [and, [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]], [or, [u_eq_c63, u_type_name, [u_ob_c36_get, u_self, [quote, type]]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_type_name, [u_ob_c36_names, u_x]]], [u_ty_c36_supertypes_xx, [u_ob_c36_get, u_self, [quote, type]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_instance_c63,
			  wl:arglist_info(u_ty_c36_instance_c63, f_u_ty_c36_instance_c63, [u_self, u_type_name], arginfo{all:[u_self, u_type_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_type_name], opt:0, req:[u_self, u_type_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_instance_c63,
			  wl:init_args(exact_only, f_u_ty_c36_instance_c63))).
*/
/*
(defun ty$instance-of? (self type)
  (and (ty? (ob$get self 'type))
       (memq? type (ty$supertypes* (ob$get self 'type)))))

;
; ppformat = (<prop | nil> <slotnames> <optional-slotnames>)
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:993 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$instance-of?',[self,type],[and,['ty?',['ob$get',self,[quote,type]]],['memq?',type,['ty$supertypes*',['ob$get',self,[quote,type]]]]]])
wl:lambda_def(defun, u_ty_c36_instance_of_c63, f_u_ty_c36_instance_of_c63, [u_self, type], [[and, [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]], [u_memq_c63, type, [u_ty_c36_supertypes_xx, [u_ob_c36_get, u_self, [quote, type]]]]]]).
wl:arglist_info(u_ty_c36_instance_of_c63, f_u_ty_c36_instance_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_instance_of_c63).

/*

### Compiled:  `U::TY$INSTANCE-OF?` 
*/
f_u_ty_c36_instance_of_c63(Self, Type, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self), bv(type, Type)|Env],
	f_u_ty_c63([u_ob_c36_get, u_self, [quote, type]], IFTEST),
	(   IFTEST\==[]
	->  f_u_memq_c63(type,
			 
			 [ u_ty_c36_supertypes_xx,
			   [u_ob_c36_get, u_self, [quote, type]]
			 ],
			 TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ty_c36_instance_of_c63, classof, claz_function),
   set_opv(u_ty_c36_instance_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_instance_of_c63, function, f_u_ty_c36_instance_of_c63),
   _Ignored4=u_ty_c36_instance_of_c63.
/*
:- side_effect(assert_lsp(u_ty_c36_instance_of_c63,
			  wl:lambda_def(defun, u_ty_c36_instance_of_c63, f_u_ty_c36_instance_of_c63, [u_self, type], [[and, [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]], [u_memq_c63, type, [u_ty_c36_supertypes_xx, [u_ob_c36_get, u_self, [quote, type]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_instance_of_c63,
			  wl:arglist_info(u_ty_c36_instance_of_c63, f_u_ty_c36_instance_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_instance_of_c63,
			  wl:init_args(exact_only, f_u_ty_c36_instance_of_c63))).
*/
/*
*/
/*
 ppformat = (<prop | nil> <slotnames> <optional-slotnames>)
*/
/*
*/
/*
(defun ty$create (name parent-names ppformat)
  (let* ((temp nil)
         (parents
          (map 'list (lambda (x)
                      (setq temp (ob$name->ob x))
                      (if (null? temp)
                          (error "ty$create "(defun ty$create (name parent-names ppformat)\n  (let* ((temp nil)\n         (parents\n          (map 'list (lambda (x)\n                      (setq temp (ob$name->ob x))\n                      (if (null? temp)\n                          (error \"ty$create ~A: ~A not defined yet.~%\" name x)\n                          temp))\n               parent-names))\n         (type\n          (ty$new name parents)))\n        (cond\n         (ppformat (ob$set type 'ppformat ppformat))\n         (parents (ob$set type 'ppformat (ob$get (car parents) 'ppformat)))\n         (t (ob$set type 'ppformat\n                       '(prop (actor from to obj) (actor from to obj)))))\n        type))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1187 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$create',[name,'parent-names',ppformat],['let*',[[temp,[]],[parents,[map,[quote,list],[lambda,[x],[setq,temp,['ob$name->ob',x]],[if,['null?',temp],[error,'$STRING'("ty$create ~A: ~A not defined yet.~%"),name,x],temp]],'parent-names']],[type,['ty$new',name,parents]]],[cond,[ppformat,['ob$set',type,[quote,ppformat],ppformat]],[parents,['ob$set',type,[quote,ppformat],['ob$get',[car,parents],[quote,ppformat]]]],[t,['ob$set',type,[quote,ppformat],[quote,[prop,[actor,from,to,obj],[actor,from,to,obj]]]]]],type]])
wl:lambda_def(defun, u_ty_c36_create, f_u_ty_c36_create, [sys_name, u_parent_names, u_ppformat], [[let_xx, [[u_temp, []], [u_parents, [map, [quote, list], [lambda, [u_x], [setq, u_temp, [u_ob_c36_name_c62_ob, u_x]], [if, [u_null_c63, u_temp], [error, '$ARRAY'([*], claz_base_character, "ty$create ~A: ~A not defined yet.~%"), sys_name, u_x], u_temp]], u_parent_names]], [type, [u_ty_c36_new, sys_name, u_parents]]], [cond, [u_ppformat, [u_ob_c36_set, type, [quote, u_ppformat], u_ppformat]], [u_parents, [u_ob_c36_set, type, [quote, u_ppformat], [u_ob_c36_get, [car, u_parents], [quote, u_ppformat]]]], [t, [u_ob_c36_set, type, [quote, u_ppformat], [quote, [u_prop, [u_actor, u_from, u_to, u_obj], [u_actor, u_from, u_to, u_obj]]]]]], type]]).
wl:arglist_info(u_ty_c36_create, f_u_ty_c36_create, [sys_name, u_parent_names, u_ppformat], arginfo{all:[sys_name, u_parent_names, u_ppformat], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_parent_names, u_ppformat], opt:0, req:[sys_name, u_parent_names, u_ppformat], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_create).

/*

### Compiled:  `U::TY$CREATE` 
*/
f_u_ty_c36_create(Name, Parent_names, Ppformat, FnResult) :-
	nop(global_env(Env)),
	Env45=[bv(sys_name, Name), bv(u_parent_names, Parent_names), bv(u_ppformat, Ppformat)|Env],
	Lambda=closure([AEnv|Env45], LResult, [u_x],  (get_var(AEnv, u_x, X_Get), f_u_ob_c36_name_c62_ob(X_Get, Temp), set_var(AEnv, u_temp, Temp), f_u_null_c63(u_temp, IFTEST), (IFTEST\==[]->get_var(AEnv, sys_name, Name_Get), get_var(AEnv, u_x, X_Get15), cl_error(['$ARRAY'([*], claz_base_character, "ty$create ~A: ~A not defined yet.~%"), Name_Get, X_Get15], TrueResult), LResult=TrueResult;get_var(AEnv, u_temp, Temp_Get), LResult=Temp_Get))),
	get_var(Env45, u_parent_names, Parent_names_Get),
	cl_map(list, Lambda, Parent_names_Get, Parents_Init),
	get_var(Env45, sys_name, Name_Get23),
	get_var(Env45, u_parents, Parents_Get),
	f_u_ty_c36_new(Name_Get23, Parents_Get, Type_Init),
	LEnv=[bv(u_temp, []), bv(u_parents, Parents_Init), bv(type, Type_Init)|Env45],
	get_var(LEnv, u_ppformat, IFTEST27),
	(   IFTEST27\==[]
	->  get_var(LEnv, type, Type_Get),
	    get_var(LEnv, u_ppformat, Ppformat_Get31),
	    f_u_ob_c36_set(Type_Get, u_ppformat, Ppformat_Get31, TrueResult40),
	    ElseResult41=TrueResult40
	;   get_var(LEnv, u_parents, IFTEST32),
	    (   IFTEST32\==[]
	    ->  get_var(LEnv, type, Type_Get35),
		get_var(LEnv, u_parents, Parents_Get36),
		cl_car(Parents_Get36, C36_get_Param),
		f_u_ob_c36_get(C36_get_Param, u_ppformat, Ppformat50),
		f_u_ob_c36_set(Type_Get35, u_ppformat, Ppformat50, TrueResult38),
		ElseResult41=TrueResult38
	    ;   get_var(LEnv, type, Type_Get37),
		f_u_ob_c36_set(Type_Get37,
			       u_ppformat,
			       
			       [ u_prop,
				 [u_actor, u_from, u_to, u_obj],
				 [u_actor, u_from, u_to, u_obj]
			       ],
			       ElseResult39),
		ElseResult41=ElseResult39
	    )
	),
	get_var(LEnv, type, Type_Get42),
	Type_Get42=FnResult.
:- set_opv(f_u_ty_c36_create, classof, claz_function),
   set_opv(u_ty_c36_create, compile_as, kw_function),
   set_opv(u_ty_c36_create, function, f_u_ty_c36_create),
   _Ignored4=u_ty_c36_create.
/*
:- side_effect(assert_lsp(u_ty_c36_create,
			  wl:lambda_def(defun, u_ty_c36_create, f_u_ty_c36_create, [sys_name, u_parent_names, u_ppformat], [[let_xx, [[u_temp, []], [u_parents, [map, [quote, list], [lambda, [u_x], [setq, u_temp, [u_ob_c36_name_c62_ob, u_x]], [if, [u_null_c63, u_temp], [error, '$ARRAY'([*], claz_base_character, "ty$create ~A: ~A not defined yet.~%"), sys_name, u_x], u_temp]], u_parent_names]], [type, [u_ty_c36_new, sys_name, u_parents]]], [cond, [u_ppformat, [u_ob_c36_set, type, [quote, u_ppformat], u_ppformat]], [u_parents, [u_ob_c36_set, type, [quote, u_ppformat], [u_ob_c36_get, [car, u_parents], [quote, u_ppformat]]]], [t, [u_ob_c36_set, type, [quote, u_ppformat], [quote, [u_prop, [u_actor, u_from, u_to, u_obj], [u_actor, u_from, u_to, u_obj]]]]]], type]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_create,
			  wl:arglist_info(u_ty_c36_create, f_u_ty_c36_create, [sys_name, u_parent_names, u_ppformat], arginfo{all:[sys_name, u_parent_names, u_ppformat], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_parent_names, u_ppformat], opt:0, req:[sys_name, u_parent_names, u_ppformat], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_create,
			  wl:init_args(exact_only, f_u_ty_c36_create))).
*/
/*
(defun ty$fcreate (name parent-names slots)
  (ty$create name parent-names (list nil slots nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1852 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$fcreate',[name,'parent-names',slots],['ty$create',name,'parent-names',[list,[],slots,[]]]])
wl:lambda_def(defun, u_ty_c36_fcreate, f_u_ty_c36_fcreate, [sys_name, u_parent_names, sys_slots], [[u_ty_c36_create, sys_name, u_parent_names, [list, [], sys_slots, []]]]).
wl:arglist_info(u_ty_c36_fcreate, f_u_ty_c36_fcreate, [sys_name, u_parent_names, sys_slots], arginfo{all:[sys_name, u_parent_names, sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_parent_names, sys_slots], opt:0, req:[sys_name, u_parent_names, sys_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_fcreate).

/*

### Compiled:  `U::TY$FCREATE` 
*/
f_u_ty_c36_fcreate(Name, Parent_names, Slots, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(sys_name, Name), bv(u_parent_names, Parent_names), bv(sys_slots, Slots)|Env],
	get_var(Env12, sys_name, Name_Get),
	get_var(Env12, sys_slots, Slots_Get),
	get_var(Env12, u_parent_names, Parent_names_Get),
	_201863972=[[], Slots_Get, []],
	f_u_ty_c36_create(Name_Get, Parent_names_Get, _201863972, C36_create_Ret),
	C36_create_Ret=FnResult.
:- set_opv(f_u_ty_c36_fcreate, classof, claz_function),
   set_opv(u_ty_c36_fcreate, compile_as, kw_function),
   set_opv(u_ty_c36_fcreate, function, f_u_ty_c36_fcreate),
   _Ignored4=u_ty_c36_fcreate.
/*
:- side_effect(assert_lsp(u_ty_c36_fcreate,
			  wl:lambda_def(defun, u_ty_c36_fcreate, f_u_ty_c36_fcreate, [sys_name, u_parent_names, sys_slots], [[u_ty_c36_create, sys_name, u_parent_names, [list, [], sys_slots, []]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_fcreate,
			  wl:arglist_info(u_ty_c36_fcreate, f_u_ty_c36_fcreate, [sys_name, u_parent_names, sys_slots], arginfo{all:[sys_name, u_parent_names, sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_parent_names, sys_slots], opt:0, req:[sys_name, u_parent_names, sys_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_fcreate,
			  wl:init_args(exact_only, f_u_ty_c36_fcreate))).
*/
/*
(defun ty$new (name supertypes)
  (let ((type (ob$create-named-empty name))
        (temp nil))
       (ob$set type 'type *ty-ob*)
       (setq *types* (cons type *types*))
       (yloop
        (yfor supertype in supertypes)
        (ydo (ob$add type 'isa supertype)))
       ; Exemplars are used by the DAYDREAMER generator.
       (setq temp (ob$create-empty))
       (ob$add temp 'type type)
       (ob$set-literal type t)
       (ob$add type 'exemplar temp)
       ; Return new type
       type))

; This is way recursive!
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1951 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$new',[name,supertypes],[let,[[type,['ob$create-named-empty',name]],[temp,[]]],['ob$set',type,[quote,type],'*ty-ob*'],[setq,'*types*',[cons,type,'*types*']],[yloop,[yfor,supertype,in,supertypes],[ydo,['ob$add',type,[quote,isa],supertype]]],[setq,temp,['ob$create-empty']],['ob$add',temp,[quote,type],type],['ob$set-literal',type,t],['ob$add',type,[quote,exemplar],temp],type]])
wl:lambda_def(defun, u_ty_c36_new, f_u_ty_c36_new, [sys_name, u_supertypes], [[let, [[type, [u_ob_c36_create_named_empty, sys_name]], [u_temp, []]], [u_ob_c36_set, type, [quote, type], u_xx_ty_ob_xx], [setq, u_xx_types_xx, [cons, type, u_xx_types_xx]], [u_yloop, [u_yfor, u_supertype, u_in, u_supertypes], [u_ydo, [u_ob_c36_add, type, [quote, u_isa], u_supertype]]], [setq, u_temp, [u_ob_c36_create_empty]], [u_ob_c36_add, u_temp, [quote, type], type], [u_ob_c36_set_literal, type, t], [u_ob_c36_add, type, [quote, u_exemplar], u_temp], type]]).
wl:arglist_info(u_ty_c36_new, f_u_ty_c36_new, [sys_name, u_supertypes], arginfo{all:[sys_name, u_supertypes], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_supertypes], opt:0, req:[sys_name, u_supertypes], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_new).

/*

### Compiled:  `U::TY$NEW` 
*/
f_u_ty_c36_new(Name, Supertypes, FnResult) :-
	nop(global_env(Env)),
	Env25=[bv(sys_name, Name), bv(u_supertypes, Supertypes)|Env],
	get_var(Env25, sys_name, Name_Get),
	f_u_ob_c36_create_named_empty(Name_Get, Type_Init),
	LEnv=[bv(type, Type_Init), bv(u_temp, [])|Env25],
	get_var(LEnv, type, Type_Get),
	get_var(LEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
	f_u_ob_c36_set(Type_Get, type, Xx_ty_ob_xx_Get, C36_set_Ret),
	get_var(LEnv, type, Type_Get15),
	get_var(LEnv, u_xx_types_xx, Xx_types_xx_Get),
	Xx_types_xx=[Type_Get15|Xx_types_xx_Get],
	set_var(LEnv, u_xx_types_xx, Xx_types_xx),
	f_u_yloop(
		  [ [u_yfor, u_supertype, u_in, u_supertypes],
		    [u_ydo, [u_ob_c36_add, type, [quote, u_isa], u_supertype]]
		  ],
		  Yloop_Ret),
	f_u_ob_c36_create_empty(Temp),
	set_var(LEnv, u_temp, Temp),
	get_var(LEnv, type, Type_Get18),
	get_var(LEnv, u_temp, Temp_Get),
	f_u_ob_c36_add(Temp_Get, type, Type_Get18, C36_add_Ret),
	get_var(LEnv, type, Type_Get19),
	f_u_ob_c36_set_literal(Type_Get19, t, T),
	get_var(LEnv, type, Type_Get20),
	get_var(LEnv, u_temp, Temp_Get21),
	f_u_ob_c36_add(Type_Get20, u_exemplar, Temp_Get21, C36_add_Ret34),
	get_var(LEnv, type, Type_Get22),
	Type_Get22=FnResult.
:- set_opv(f_u_ty_c36_new, classof, claz_function),
   set_opv(u_ty_c36_new, compile_as, kw_function),
   set_opv(u_ty_c36_new, function, f_u_ty_c36_new),
   _Ignored4=u_ty_c36_new.
/*
:- side_effect(assert_lsp(u_ty_c36_new,
			  wl:lambda_def(defun, u_ty_c36_new, f_u_ty_c36_new, [sys_name, u_supertypes], [[let, [[type, [u_ob_c36_create_named_empty, sys_name]], [u_temp, []]], [u_ob_c36_set, type, [quote, type], u_xx_ty_ob_xx], [setq, u_xx_types_xx, [cons, type, u_xx_types_xx]], [u_yloop, [u_yfor, u_supertype, u_in, u_supertypes], [u_ydo, [u_ob_c36_add, type, [quote, u_isa], u_supertype]]], [setq, u_temp, [u_ob_c36_create_empty]], [u_ob_c36_add, u_temp, [quote, type], type], [u_ob_c36_set_literal, type, t], [u_ob_c36_add, type, [quote, u_exemplar], u_temp], type]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_new,
			  wl:arglist_info(u_ty_c36_new, f_u_ty_c36_new, [sys_name, u_supertypes], arginfo{all:[sys_name, u_supertypes], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, u_supertypes], opt:0, req:[sys_name, u_supertypes], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_new,
			  wl:init_args(exact_only, f_u_ty_c36_new))).
*/
/*
 Exemplars are used by the DAYDREAMER generator.
*/
/*
 Return new type
*/
/*
 This is way recursive!
*/
/*
(setq *ty-ob* (ob$create-named-empty 'ty))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2479 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ty-ob*',['ob$create-named-empty',[quote,ty]]])
:- f_u_ob_c36_create_named_empty(u_ty, _Ignored4),
   set_var(AEnv, u_xx_ty_ob_xx, _Ignored4).
/*
(ob$set *ty-ob* 'ppformat '(nil (exemplar)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2522 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$set','*ty-ob*',[quote,ppformat],[quote,[[],[exemplar]]]])
:- get_var(GEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   f_u_ob_c36_set(Xx_ty_ob_xx_Get, u_ppformat, [[], [u_exemplar]], _Ignored4).
/*
(ob$set-literal *ty-ob* t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2567 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$set-literal','*ty-ob*',t])
:- get_var(GEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   f_u_ob_c36_set_literal(Xx_ty_ob_xx_Get, t, _Ignored4).
/*
(setq *types* (list *ty-ob*))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2595 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*types*',[list,'*ty-ob*']])
:- get_var(AEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   _Ignored4=[Xx_ty_ob_xx_Get],
   set_var(AEnv, u_xx_types_xx, _Ignored4).
/*
(ob$set *ty-ob* 'type *ty-ob*)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2625 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$set','*ty-ob*',[quote,type],'*ty-ob*'])
:- get_var(GEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get6),
   f_u_ob_c36_set(Xx_ty_ob_xx_Get6, type, Xx_ty_ob_xx_Get6, _Ignored4).
/*
(defun ty$major-type (type-name)
  (ob$set (ob$name->ob type-name) 'major-type? t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2657 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$major-type',['type-name'],['ob$set',['ob$name->ob','type-name'],[quote,'major-type?'],t]])
wl:lambda_def(defun, u_ty_c36_major_type, f_u_ty_c36_major_type, [u_type_name], [[u_ob_c36_set, [u_ob_c36_name_c62_ob, u_type_name], [quote, u_major_type_c63], t]]).
wl:arglist_info(u_ty_c36_major_type, f_u_ty_c36_major_type, [u_type_name], arginfo{all:[u_type_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type_name], opt:0, req:[u_type_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_major_type).

/*

### Compiled:  `U::TY$MAJOR-TYPE` 
*/
f_u_ty_c36_major_type(Type_name, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_type_name, Type_name)|Env],
	get_var(Env10, u_type_name, Type_name_Get),
	f_u_ob_c36_name_c62_ob(Type_name_Get, C36_set_Param),
	f_u_ob_c36_set(C36_set_Param, u_major_type_c63, t, T),
	T=FnResult.
:- set_opv(f_u_ty_c36_major_type, classof, claz_function),
   set_opv(u_ty_c36_major_type, compile_as, kw_function),
   set_opv(u_ty_c36_major_type, function, f_u_ty_c36_major_type),
   _Ignored4=u_ty_c36_major_type.
/*
:- side_effect(assert_lsp(u_ty_c36_major_type,
			  wl:lambda_def(defun, u_ty_c36_major_type, f_u_ty_c36_major_type, [u_type_name], [[u_ob_c36_set, [u_ob_c36_name_c62_ob, u_type_name], [quote, u_major_type_c63], t]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_major_type,
			  wl:arglist_info(u_ty_c36_major_type, f_u_ty_c36_major_type, [u_type_name], arginfo{all:[u_type_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type_name], opt:0, req:[u_type_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_major_type,
			  wl:init_args(exact_only, f_u_ty_c36_major_type))).
*/
/*
(defun ty$display ()
  (yloop (yfor type in *types*)
         (ydo (ob$unhide type))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2742 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$display',[],[yloop,[yfor,type,in,'*types*'],[ydo,['ob$unhide',type]]]])
wl:lambda_def(defun, u_ty_c36_display, f_u_ty_c36_display, [], [[u_yloop, [u_yfor, type, u_in, u_xx_types_xx], [u_ydo, [u_ob_c36_unhide, type]]]]).
wl:arglist_info(u_ty_c36_display, f_u_ty_c36_display, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_display).

/*

### Compiled:  `U::TY$DISPLAY` 
*/
f_u_ty_c36_display(FnResult) :-
	nop(global_env(Env)),
	_204621938=Env,
	f_u_yloop(
		  [ [u_yfor, type, u_in, u_xx_types_xx],
		    [u_ydo, [u_ob_c36_unhide, type]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_display, classof, claz_function),
   set_opv(u_ty_c36_display, compile_as, kw_function),
   set_opv(u_ty_c36_display, function, f_u_ty_c36_display),
   _Ignored4=u_ty_c36_display.
/*
:- side_effect(assert_lsp(u_ty_c36_display,
			  wl:lambda_def(defun, u_ty_c36_display, f_u_ty_c36_display, [], [[u_yloop, [u_yfor, type, u_in, u_xx_types_xx], [u_ydo, [u_ob_c36_unhide, type]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_display,
			  wl:arglist_info(u_ty_c36_display, f_u_ty_c36_display, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_display,
			  wl:init_args(exact_only, f_u_ty_c36_display))).
*/
/*
(defun ty$supertypes (self)
  (ob$gets self 'isa))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2830 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$supertypes',[self],['ob$gets',self,[quote,isa]]])
wl:lambda_def(defun, u_ty_c36_supertypes, f_u_ty_c36_supertypes, [u_self], [[u_ob_c36_gets, u_self, [quote, u_isa]]]).
wl:arglist_info(u_ty_c36_supertypes, f_u_ty_c36_supertypes, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_supertypes).

/*

### Compiled:  `U::TY$SUPERTYPES` 
*/
f_u_ty_c36_supertypes(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_gets(Self_Get, u_isa, Isa),
	Isa=FnResult.
:- set_opv(f_u_ty_c36_supertypes, classof, claz_function),
   set_opv(u_ty_c36_supertypes, compile_as, kw_function),
   set_opv(u_ty_c36_supertypes, function, f_u_ty_c36_supertypes),
   _Ignored4=u_ty_c36_supertypes.
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes,
			  wl:lambda_def(defun, u_ty_c36_supertypes, f_u_ty_c36_supertypes, [u_self], [[u_ob_c36_gets, u_self, [quote, u_isa]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes,
			  wl:arglist_info(u_ty_c36_supertypes, f_u_ty_c36_supertypes, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes,
			  wl:init_args(exact_only, f_u_ty_c36_supertypes))).
*/
/*
(defun ty$subtypes (self)
  (ob$gets self 'isa-of))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2882 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$subtypes',[self],['ob$gets',self,[quote,'isa-of']]])
wl:lambda_def(defun, u_ty_c36_subtypes, f_u_ty_c36_subtypes, [u_self], [[u_ob_c36_gets, u_self, [quote, u_isa_of]]]).
wl:arglist_info(u_ty_c36_subtypes, f_u_ty_c36_subtypes, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_subtypes).

/*

### Compiled:  `U::TY$SUBTYPES` 
*/
f_u_ty_c36_subtypes(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_gets(Self_Get, u_isa_of, Isa_of),
	Isa_of=FnResult.
:- set_opv(f_u_ty_c36_subtypes, classof, claz_function),
   set_opv(u_ty_c36_subtypes, compile_as, kw_function),
   set_opv(u_ty_c36_subtypes, function, f_u_ty_c36_subtypes),
   _Ignored4=u_ty_c36_subtypes.
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes,
			  wl:lambda_def(defun, u_ty_c36_subtypes, f_u_ty_c36_subtypes, [u_self], [[u_ob_c36_gets, u_self, [quote, u_isa_of]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes,
			  wl:arglist_info(u_ty_c36_subtypes, f_u_ty_c36_subtypes, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes,
			  wl:init_args(exact_only, f_u_ty_c36_subtypes))).
*/
/*
(defun ty$supertypes* (self)
  (yloop (initial (result nil)
                  (x nil))
        (yfor type in (ob$gets self 'isa))
        (ydo (setq x (ty$supertypes* type))
             (if (not (null? x))
                 (setq result (union result x))))
        (yresult (cons self result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2935 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$supertypes*',[self],[yloop,[initial,[result,[]],[x,[]]],[yfor,type,in,['ob$gets',self,[quote,isa]]],[ydo,[setq,x,['ty$supertypes*',type]],[if,[not,['null?',x]],[setq,result,[union,result,x]]]],[yresult,[cons,self,result]]]])
wl:lambda_def(defun, u_ty_c36_supertypes_xx, f_u_ty_c36_supertypes_xx, [u_self], [[u_yloop, [u_initial, [u_result, []], [u_x, []]], [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa]]], [u_ydo, [setq, u_x, [u_ty_c36_supertypes_xx, type]], [if, [not, [u_null_c63, u_x]], [setq, u_result, [union, u_result, u_x]]]], [u_yresult, [cons, u_self, u_result]]]]).
wl:arglist_info(u_ty_c36_supertypes_xx, f_u_ty_c36_supertypes_xx, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_supertypes_xx).

/*

### Compiled:  `U::TY$SUPERTYPES*` 
*/
f_u_ty_c36_supertypes_xx(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_x, []]],
		    [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa]]],
		    
		    [ u_ydo,
		      [setq, u_x, [u_ty_c36_supertypes_xx, type]],
		      
		      [ if,
			[not, [u_null_c63, u_x]],
			[setq, u_result, [union, u_result, u_x]]
		      ]
		    ],
		    [u_yresult, [cons, u_self, u_result]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_supertypes_xx, classof, claz_function),
   set_opv(u_ty_c36_supertypes_xx, compile_as, kw_function),
   set_opv(u_ty_c36_supertypes_xx, function, f_u_ty_c36_supertypes_xx),
   _Ignored4=u_ty_c36_supertypes_xx.
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes_xx,
			  wl:lambda_def(defun, u_ty_c36_supertypes_xx, f_u_ty_c36_supertypes_xx, [u_self], [[u_yloop, [u_initial, [u_result, []], [u_x, []]], [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa]]], [u_ydo, [setq, u_x, [u_ty_c36_supertypes_xx, type]], [if, [not, [u_null_c63, u_x]], [setq, u_result, [union, u_result, u_x]]]], [u_yresult, [cons, u_self, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes_xx,
			  wl:arglist_info(u_ty_c36_supertypes_xx, f_u_ty_c36_supertypes_xx, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertypes_xx,
			  wl:init_args(exact_only, f_u_ty_c36_supertypes_xx))).
*/
/*
(defun ty$supertype-of? (self type)
  (memq? type (ty$supertypes* self)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3232 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$supertype-of?',[self,type],['memq?',type,['ty$supertypes*',self]]])
wl:lambda_def(defun, u_ty_c36_supertype_of_c63, f_u_ty_c36_supertype_of_c63, [u_self, type], [[u_memq_c63, type, [u_ty_c36_supertypes_xx, u_self]]]).
wl:arglist_info(u_ty_c36_supertype_of_c63, f_u_ty_c36_supertype_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_supertype_of_c63).

/*

### Compiled:  `U::TY$SUPERTYPE-OF?` 
*/
f_u_ty_c36_supertype_of_c63(Self, Type, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(type, Type)|Env],
	f_u_memq_c63(type, [u_ty_c36_supertypes_xx, u_self], Memq_c63_Ret),
	Memq_c63_Ret=FnResult.
:- set_opv(f_u_ty_c36_supertype_of_c63, classof, claz_function),
   set_opv(u_ty_c36_supertype_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_supertype_of_c63, function, f_u_ty_c36_supertype_of_c63),
   _Ignored4=u_ty_c36_supertype_of_c63.
/*
:- side_effect(assert_lsp(u_ty_c36_supertype_of_c63,
			  wl:lambda_def(defun, u_ty_c36_supertype_of_c63, f_u_ty_c36_supertype_of_c63, [u_self, type], [[u_memq_c63, type, [u_ty_c36_supertypes_xx, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertype_of_c63,
			  wl:arglist_info(u_ty_c36_supertype_of_c63, f_u_ty_c36_supertype_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_supertype_of_c63,
			  wl:init_args(exact_only, f_u_ty_c36_supertype_of_c63))).
*/
/*
(defun ty$subtypes* (self)
  (yloop (initial (result nil))
        (yfor type in (ob$gets self 'isa-of))
        (ydo (setq result (append result (ty$subtypes* type))))
        (yresult (cons self result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3307 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$subtypes*',[self],[yloop,[initial,[result,[]]],[yfor,type,in,['ob$gets',self,[quote,'isa-of']]],[ydo,[setq,result,[append,result,['ty$subtypes*',type]]]],[yresult,[cons,self,result]]]])
wl:lambda_def(defun, u_ty_c36_subtypes_xx, f_u_ty_c36_subtypes_xx, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa_of]]], [u_ydo, [setq, u_result, [append, u_result, [u_ty_c36_subtypes_xx, type]]]], [u_yresult, [cons, u_self, u_result]]]]).
wl:arglist_info(u_ty_c36_subtypes_xx, f_u_ty_c36_subtypes_xx, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_subtypes_xx).

/*

### Compiled:  `U::TY$SUBTYPES*` 
*/
f_u_ty_c36_subtypes_xx(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa_of]]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[append, u_result, [u_ty_c36_subtypes_xx, type]]
		      ]
		    ],
		    [u_yresult, [cons, u_self, u_result]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_subtypes_xx, classof, claz_function),
   set_opv(u_ty_c36_subtypes_xx, compile_as, kw_function),
   set_opv(u_ty_c36_subtypes_xx, function, f_u_ty_c36_subtypes_xx),
   _Ignored4=u_ty_c36_subtypes_xx.
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes_xx,
			  wl:lambda_def(defun, u_ty_c36_subtypes_xx, f_u_ty_c36_subtypes_xx, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa_of]]], [u_ydo, [setq, u_result, [append, u_result, [u_ty_c36_subtypes_xx, type]]]], [u_yresult, [cons, u_self, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes_xx,
			  wl:arglist_info(u_ty_c36_subtypes_xx, f_u_ty_c36_subtypes_xx, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtypes_xx,
			  wl:init_args(exact_only, f_u_ty_c36_subtypes_xx))).
*/
/*
(defun ty$subtype-of? (self type)
  (memq? type (ty$subtypes* self)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3516 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$subtype-of?',[self,type],['memq?',type,['ty$subtypes*',self]]])
wl:lambda_def(defun, u_ty_c36_subtype_of_c63, f_u_ty_c36_subtype_of_c63, [u_self, type], [[u_memq_c63, type, [u_ty_c36_subtypes_xx, u_self]]]).
wl:arglist_info(u_ty_c36_subtype_of_c63, f_u_ty_c36_subtype_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_subtype_of_c63).

/*

### Compiled:  `U::TY$SUBTYPE-OF?` 
*/
f_u_ty_c36_subtype_of_c63(Self, Type, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(type, Type)|Env],
	f_u_memq_c63(type, [u_ty_c36_subtypes_xx, u_self], Memq_c63_Ret),
	Memq_c63_Ret=FnResult.
:- set_opv(f_u_ty_c36_subtype_of_c63, classof, claz_function),
   set_opv(u_ty_c36_subtype_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_subtype_of_c63, function, f_u_ty_c36_subtype_of_c63),
   _Ignored4=u_ty_c36_subtype_of_c63.
/*
:- side_effect(assert_lsp(u_ty_c36_subtype_of_c63,
			  wl:lambda_def(defun, u_ty_c36_subtype_of_c63, f_u_ty_c36_subtype_of_c63, [u_self, type], [[u_memq_c63, type, [u_ty_c36_subtypes_xx, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtype_of_c63,
			  wl:arglist_info(u_ty_c36_subtype_of_c63, f_u_ty_c36_subtype_of_c63, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_subtype_of_c63,
			  wl:init_args(exact_only, f_u_ty_c36_subtype_of_c63))).
*/
/*
(defun ty$least-common-supertype (type1 type2)
  (yloop (initial (supertypes*2 (ty$supertypes* type2))
                 (result nil))
        (yfor supertype1 in (ty$supertypes* type1))
        (yuntil result)
        (ydo (if (memq? supertype1 supertypes*2)
                (setq result supertype1)))
        (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3587 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$least-common-supertype',[type1,type2],[yloop,[initial,['supertypes*2',['ty$supertypes*',type2]],[result,[]]],[yfor,supertype1,in,['ty$supertypes*',type1]],[yuntil,result],[ydo,[if,['memq?',supertype1,'supertypes*2'],[setq,result,supertype1]]],[yresult,result]]])
wl:lambda_def(defun, u_ty_c36_least_common_supertype, f_u_ty_c36_least_common_supertype, [u_type1, u_type2], [[u_yloop, [u_initial, [u_supertypes_xx_2, [u_ty_c36_supertypes_xx, u_type2]], [u_result, []]], [u_yfor, u_supertype1, u_in, [u_ty_c36_supertypes_xx, u_type1]], [u_yuntil, u_result], [u_ydo, [if, [u_memq_c63, u_supertype1, u_supertypes_xx_2], [setq, u_result, u_supertype1]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ty_c36_least_common_supertype, f_u_ty_c36_least_common_supertype, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_least_common_supertype).

/*

### Compiled:  `U::TY$LEAST-COMMON-SUPERTYPE` 
*/
f_u_ty_c36_least_common_supertype(Type1, Type2, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_type1, Type1), bv(u_type2, Type2)|Env],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_supertypes_xx_2, [u_ty_c36_supertypes_xx, u_type2]],
		      [u_result, []]
		    ],
		    
		    [ u_yfor,
		      u_supertype1,
		      u_in,
		      [u_ty_c36_supertypes_xx, u_type1]
		    ],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_memq_c63, u_supertype1, u_supertypes_xx_2],
			[setq, u_result, u_supertype1]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_least_common_supertype, classof, claz_function),
   set_opv(u_ty_c36_least_common_supertype, compile_as, kw_function),
   set_opv(u_ty_c36_least_common_supertype,
	   function,
	   f_u_ty_c36_least_common_supertype),
   _Ignored4=u_ty_c36_least_common_supertype.
/*
:- side_effect(assert_lsp(u_ty_c36_least_common_supertype,
			  wl:lambda_def(defun, u_ty_c36_least_common_supertype, f_u_ty_c36_least_common_supertype, [u_type1, u_type2], [[u_yloop, [u_initial, [u_supertypes_xx_2, [u_ty_c36_supertypes_xx, u_type2]], [u_result, []]], [u_yfor, u_supertype1, u_in, [u_ty_c36_supertypes_xx, u_type1]], [u_yuntil, u_result], [u_ydo, [if, [u_memq_c63, u_supertype1, u_supertypes_xx_2], [setq, u_result, u_supertype1]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_least_common_supertype,
			  wl:arglist_info(u_ty_c36_least_common_supertype, f_u_ty_c36_least_common_supertype, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_least_common_supertype,
			  wl:init_args(exact_only, f_u_ty_c36_least_common_supertype))).
*/
/*
(defun ty$basic-type-distance (ancestor type)
  (let ((position (position ancestor (ty$supertypes* type))))
       (if position (+ 1 position) *max-fixnum*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3917 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$basic-type-distance',[ancestor,type],[let,[[position,[position,ancestor,['ty$supertypes*',type]]]],[if,position,[+,1,position],'*max-fixnum*']]])
wl:lambda_def(defun, u_ty_c36_basic_type_distance, f_u_ty_c36_basic_type_distance, [u_ancestor, type], [[let, [[position, [position, u_ancestor, [u_ty_c36_supertypes_xx, type]]]], [if, position, [+, 1, position], u_xx_max_fixnum_xx]]]).
wl:arglist_info(u_ty_c36_basic_type_distance, f_u_ty_c36_basic_type_distance, [u_ancestor, type], arginfo{all:[u_ancestor, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ancestor, type], opt:0, req:[u_ancestor, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_basic_type_distance).

/*

### Compiled:  `U::TY$BASIC-TYPE-DISTANCE` 
*/
f_u_ty_c36_basic_type_distance(Ancestor, Type, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_ancestor, Ancestor), bv(type, Type)|Env],
	get_var(Env22, type, Type_Get),
	get_var(Env22, u_ancestor, Ancestor_Get),
	f_u_ty_c36_supertypes_xx(Type_Get, Supertypes_xx_Ret),
	cl_position(Ancestor_Get, Supertypes_xx_Ret, Position_Init),
	LEnv=[bv(position, Position_Init)|Env22],
	get_var(LEnv, position, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, position, Position_Get16),
	    +(1, Position_Get16, TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_xx_max_fixnum_xx, Xx_max_fixnum_xx_Get),
	    FnResult=Xx_max_fixnum_xx_Get
	).
:- set_opv(f_u_ty_c36_basic_type_distance, classof, claz_function),
   set_opv(u_ty_c36_basic_type_distance, compile_as, kw_function),
   set_opv(u_ty_c36_basic_type_distance,
	   function,
	   f_u_ty_c36_basic_type_distance),
   _Ignored4=u_ty_c36_basic_type_distance.
/*
:- side_effect(assert_lsp(u_ty_c36_basic_type_distance,
			  wl:lambda_def(defun, u_ty_c36_basic_type_distance, f_u_ty_c36_basic_type_distance, [u_ancestor, type], [[let, [[position, [position, u_ancestor, [u_ty_c36_supertypes_xx, type]]]], [if, position, [+, 1, position], u_xx_max_fixnum_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_basic_type_distance,
			  wl:arglist_info(u_ty_c36_basic_type_distance, f_u_ty_c36_basic_type_distance, [u_ancestor, type], arginfo{all:[u_ancestor, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ancestor, type], opt:0, req:[u_ancestor, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_basic_type_distance,
			  wl:init_args(exact_only, f_u_ty_c36_basic_type_distance))).
*/
/*
(defun ty$distance (type1 type2)
  (let ((lcs (ty$least-common-supertype type1 type2)))
       (if lcs
           (min (ty$basic-type-distance lcs type1)
                (ty$basic-type-distance lcs type2))
           *max-fixnum*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:4077 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$distance',[type1,type2],[let,[[lcs,['ty$least-common-supertype',type1,type2]]],[if,lcs,[min,['ty$basic-type-distance',lcs,type1],['ty$basic-type-distance',lcs,type2]],'*max-fixnum*']]])
wl:lambda_def(defun, u_ty_c36_distance, f_u_ty_c36_distance, [u_type1, u_type2], [[let, [[u_lcs, [u_ty_c36_least_common_supertype, u_type1, u_type2]]], [if, u_lcs, [min, [u_ty_c36_basic_type_distance, u_lcs, u_type1], [u_ty_c36_basic_type_distance, u_lcs, u_type2]], u_xx_max_fixnum_xx]]]).
wl:arglist_info(u_ty_c36_distance, f_u_ty_c36_distance, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_distance).

/*

### Compiled:  `U::TY$DISTANCE` 
*/
f_u_ty_c36_distance(Type1, Type2, FnResult) :-
	nop(global_env(Env)),
	Env25=[bv(u_type1, Type1), bv(u_type2, Type2)|Env],
	get_var(Env25, u_type1, Type1_Get),
	get_var(Env25, u_type2, Type2_Get),
	f_u_ty_c36_least_common_supertype(Type1_Get, Type2_Get, Lcs_Init),
	LEnv=[bv(u_lcs, Lcs_Init)|Env25],
	get_var(LEnv, u_lcs, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_lcs, Lcs_Get16),
	    get_var(LEnv, u_type1, Type1_Get17),
	    f_u_ty_c36_basic_type_distance(Lcs_Get16, Type1_Get17, Min_Param),
	    get_var(LEnv, u_lcs, Lcs_Get18),
	    get_var(LEnv, u_type2, Type2_Get19),
	    f_u_ty_c36_basic_type_distance(Lcs_Get18,
					   Type2_Get19,
					   Type_distance_Ret),
	    cl_min(Min_Param, Type_distance_Ret, TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_xx_max_fixnum_xx, Xx_max_fixnum_xx_Get),
	    FnResult=Xx_max_fixnum_xx_Get
	).
:- set_opv(f_u_ty_c36_distance, classof, claz_function),
   set_opv(u_ty_c36_distance, compile_as, kw_function),
   set_opv(u_ty_c36_distance, function, f_u_ty_c36_distance),
   _Ignored4=u_ty_c36_distance.
/*
:- side_effect(assert_lsp(u_ty_c36_distance,
			  wl:lambda_def(defun, u_ty_c36_distance, f_u_ty_c36_distance, [u_type1, u_type2], [[let, [[u_lcs, [u_ty_c36_least_common_supertype, u_type1, u_type2]]], [if, u_lcs, [min, [u_ty_c36_basic_type_distance, u_lcs, u_type1], [u_ty_c36_basic_type_distance, u_lcs, u_type2]], u_xx_max_fixnum_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_distance,
			  wl:arglist_info(u_ty_c36_distance, f_u_ty_c36_distance, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_distance,
			  wl:init_args(exact_only, f_u_ty_c36_distance))).
*/
/*
(defun ty$get-major-type (self)
  (yloop (initial (result nil))
         (yfor type in (ty$supertypes* self))
         (ywhile (not result))
         (ydo (setq result (if (ob$get type 'major-type?)
                                type
                                nil)))
         (yresult (if result result self))))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:4311 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty$get-major-type',[self],[yloop,[initial,[result,[]]],[yfor,type,in,['ty$supertypes*',self]],[ywhile,[not,result]],[ydo,[setq,result,[if,['ob$get',type,[quote,'major-type?']],type,[]]]],[yresult,[if,result,result,self]]]])
wl:lambda_def(defun, u_ty_c36_get_major_type, f_u_ty_c36_get_major_type, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, type, u_in, [u_ty_c36_supertypes_xx, u_self]], [u_ywhile, [not, u_result]], [u_ydo, [setq, u_result, [if, [u_ob_c36_get, type, [quote, u_major_type_c63]], type, []]]], [u_yresult, [if, u_result, u_result, u_self]]]]).
wl:arglist_info(u_ty_c36_get_major_type, f_u_ty_c36_get_major_type, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c36_get_major_type).

/*

### Compiled:  `U::TY$GET-MAJOR-TYPE` 
*/
f_u_ty_c36_get_major_type(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, type, u_in, [u_ty_c36_supertypes_xx, u_self]],
		    [u_ywhile, [not, u_result]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ if,
			  [u_ob_c36_get, type, [quote, u_major_type_c63]],
			  type,
			  []
			]
		      ]
		    ],
		    [u_yresult, [if, u_result, u_result, u_self]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_get_major_type, classof, claz_function),
   set_opv(u_ty_c36_get_major_type, compile_as, kw_function),
   set_opv(u_ty_c36_get_major_type, function, f_u_ty_c36_get_major_type),
   _Ignored4=u_ty_c36_get_major_type.
/*
:- side_effect(assert_lsp(u_ty_c36_get_major_type,
			  wl:lambda_def(defun, u_ty_c36_get_major_type, f_u_ty_c36_get_major_type, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, type, u_in, [u_ty_c36_supertypes_xx, u_self]], [u_ywhile, [not, u_result]], [u_ydo, [setq, u_result, [if, [u_ob_c36_get, type, [quote, u_major_type_c63]], type, []]]], [u_yresult, [if, u_result, u_result, u_self]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_get_major_type,
			  wl:arglist_info(u_ty_c36_get_major_type, f_u_ty_c36_get_major_type, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c36_get_major_type,
			  wl:init_args(exact_only, f_u_ty_c36_get_major_type))).
*/
/*
 End of file.
*/


%; Total compilation time: 3.26 seconds


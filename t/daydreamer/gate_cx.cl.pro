#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_cx" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:55 2017

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
 Context mechanism for obs
*/
/*
*/
/*
 6/22/85: Original version written
*/
/*
 6/30/85: Added rule comments
*/
/*
  1/6/86: Removed old rules and truth maintenance
*/
/*
 1/24/86: Added inheriting of mutations-tried?
*/
/*
 1/25/86: Added pseudo-sprouts
*/
/*
 1/27/86: Added and tested type hashing
*/
/*
 7/19/86: Added touched-facts
*/
/*
 9/24/86: Rewrote code to be flavorless
*/
/*
*/
/*
 Todo: must enforce first arg being a context.
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *cx-ob* (ty$create 'CX nil nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:697 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*cx-ob*',['ty$create',[quote,'CX'],[],[]]])
:- f_u_ty_c36_create(u_cx, [], [], _Ignored4),
   set_var(AEnv, u_xx_cx_ob_xx, _Ignored4).
/*
(setq *next-cx-number* 1)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:739 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*next-cx-number*',1])
:- set_var(AEnv, setq, u_xx_next_cx_number_xx, 1).
/*
(defun cx$create ()
  (cx$sprout nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:766 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$create',[],['cx$sprout',[]]])
wl:lambda_def(defun, u_cx_c36_create, f_u_cx_c36_create, [], [[u_cx_c36_sprout, []]]).
wl:arglist_info(u_cx_c36_create, f_u_cx_c36_create, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_create).

/*

### Compiled:  `U::CX$CREATE` 
*/
f_u_cx_c36_create(FnResult) :-
	nop(global_env(Env)),
	_216465876=Env,
	f_u_cx_c36_sprout([], C36_sprout_Ret),
	C36_sprout_Ret=FnResult.
:- set_opv(f_u_cx_c36_create, classof, claz_function),
   set_opv(u_cx_c36_create, compile_as, kw_function),
   set_opv(u_cx_c36_create, function, f_u_cx_c36_create),
   _Ignored4=u_cx_c36_create.
/*
:- side_effect(assert_lsp(u_cx_c36_create,
			  wl:lambda_def(defun, u_cx_c36_create, f_u_cx_c36_create, [], [[u_cx_c36_sprout, []]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_create,
			  wl:arglist_info(u_cx_c36_create, f_u_cx_c36_create, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_create,
			  wl:init_args(exact_only, f_u_cx_c36_create))).
*/
/*
(defun cx$sprout (parent)
  (let ((self (ob$fcreate '(CX))))
       (ob$add-unique-name self (string->symbol
         (string-append "CX." (prog1 (fixnum->string *next-cx-number*)
                                      (increment-me *next-cx-number*)))))
       (cond
        ((cx? parent)
         (ob$set self 'parent parent) (cx$add-child parent self)
         (ob$set self 'mutations-tried? (ob$get parent 'mutations-tried?))
         (if (ob$get parent 'timeout)
             (ob$set self 'timeout (- (ob$get parent 'timeout) 1)))
         (ob$set self 'pseudo-sprout? (ob$get parent 'pseudo-sprout?))
         (ob$set self 'ancestors (cons parent (ob$get parent 'ancestors)))
         (ob$set self 'all-obs (copy-list (ob$get parent 'all-obs)))
         (ob$set self 'type-hashing (map 'list (lambda (x) (copy-list x))
                                         (ob$get parent 'type-hashing)))
         (ob$set self 'gen-switches (ob$get parent 'gen-switches))
         (ob$set self 'touched-facts (ob$get parent 'touched-facts)))
        ((null? parent))
        (else (error "cx$sprout: parent is not a context or NIL.")))
       (if parent
           (ndbg *gate-dbg* context ""(defun cx$sprout (parent)\n  (let ((self (ob$fcreate '(CX))))\n       (ob$add-unique-name self (string->symbol\n         (string-append \"CX.\" (prog1 (fixnum->string *next-cx-number*)\n                                      (increment-me *next-cx-number*)))))\n       (cond\n        ((cx? parent)\n         (ob$set self 'parent parent) (cx$add-child parent self)\n         (ob$set self 'mutations-tried? (ob$get parent 'mutations-tried?))\n         (if (ob$get parent 'timeout)\n             (ob$set self 'timeout (- (ob$get parent 'timeout) 1)))\n         (ob$set self 'pseudo-sprout? (ob$get parent 'pseudo-sprout?))\n         (ob$set self 'ancestors (cons parent (ob$get parent 'ancestors)))\n         (ob$set self 'all-obs (copy-list (ob$get parent 'all-obs)))\n         (ob$set self 'type-hashing (map 'list (lambda (x) (copy-list x))\n                                         (ob$get parent 'type-hashing)))\n         (ob$set self 'gen-switches (ob$get parent 'gen-switches))\n         (ob$set self 'touched-facts (ob$get parent 'touched-facts)))\n        ((null? parent))\n        (else (error \"cx$sprout: parent is not a context or NIL.\")))\n       (if parent\n           (ndbg *gate-dbg* context \"~A --> ~A~%\" parent self))\n       self))\n\n;\n; Getters\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:806 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$sprout',[parent],[let,[[self,['ob$fcreate',[quote,['CX']]]]],['ob$add-unique-name',self,['string->symbol',['string-append','$STRING'("CX."),[prog1,['fixnum->string','*next-cx-number*'],['increment-me','*next-cx-number*']]]]],[cond,[['cx?',parent],['ob$set',self,[quote,parent],parent],['cx$add-child',parent,self],['ob$set',self,[quote,'mutations-tried?'],['ob$get',parent,[quote,'mutations-tried?']]],[if,['ob$get',parent,[quote,timeout]],['ob$set',self,[quote,timeout],[-,['ob$get',parent,[quote,timeout]],1]]],['ob$set',self,[quote,'pseudo-sprout?'],['ob$get',parent,[quote,'pseudo-sprout?']]],['ob$set',self,[quote,ancestors],[cons,parent,['ob$get',parent,[quote,ancestors]]]],['ob$set',self,[quote,'all-obs'],['copy-list',['ob$get',parent,[quote,'all-obs']]]],['ob$set',self,[quote,'type-hashing'],[map,[quote,list],[lambda,[x],['copy-list',x]],['ob$get',parent,[quote,'type-hashing']]]],['ob$set',self,[quote,'gen-switches'],['ob$get',parent,[quote,'gen-switches']]],['ob$set',self,[quote,'touched-facts'],['ob$get',parent,[quote,'touched-facts']]]],[['null?',parent]],[else,[error,'$STRING'("cx$sprout: parent is not a context or NIL.")]]],[if,parent,[ndbg,'*gate-dbg*',context,'$STRING'("~A --> ~A~%"),parent,self]],self]])
wl:lambda_def(defun, u_cx_c36_sprout, f_u_cx_c36_sprout, [u_parent], [[let, [[u_self, [u_ob_c36_fcreate, [quote, [u_cx]]]]], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "CX."), [prog1, [u_fixnum_c62_string, u_xx_next_cx_number_xx], [u_increment_me, u_xx_next_cx_number_xx]]]]], [cond, [[u_cx_c63, u_parent], [u_ob_c36_set, u_self, [quote, u_parent], u_parent], [u_cx_c36_add_child, u_parent, u_self], [u_ob_c36_set, u_self, [quote, u_mutations_tried_c63], [u_ob_c36_get, u_parent, [quote, u_mutations_tried_c63]]], [if, [u_ob_c36_get, u_parent, [quote, ext_timeout]], [u_ob_c36_set, u_self, [quote, ext_timeout], [-, [u_ob_c36_get, u_parent, [quote, ext_timeout]], 1]]], [u_ob_c36_set, u_self, [quote, u_pseudo_sprout_c63], [u_ob_c36_get, u_parent, [quote, u_pseudo_sprout_c63]]], [u_ob_c36_set, u_self, [quote, u_ancestors], [cons, u_parent, [u_ob_c36_get, u_parent, [quote, u_ancestors]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [copy_list, [u_ob_c36_get, u_parent, [quote, u_all_obs]]]], [u_ob_c36_set, u_self, [quote, u_type_hashing], [map, [quote, list], [lambda, [u_x], [copy_list, u_x]], [u_ob_c36_get, u_parent, [quote, u_type_hashing]]]], [u_ob_c36_set, u_self, [quote, u_gen_switches], [u_ob_c36_get, u_parent, [quote, u_gen_switches]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_ob_c36_get, u_parent, [quote, u_touched_facts]]]], [[u_null_c63, u_parent]], [u_else, [error, '$ARRAY'([*], claz_base_character, "cx$sprout: parent is not a context or NIL.")]]], [if, u_parent, [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "~A --> ~A~%"), u_parent, u_self]], u_self]]).
wl:arglist_info(u_cx_c36_sprout, f_u_cx_c36_sprout, [u_parent], arginfo{all:[u_parent], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_parent], opt:0, req:[u_parent], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_sprout).

/*

### Compiled:  `U::CX$SPROUT` 
*/
f_u_cx_c36_sprout(Parent, FnResult) :-
	nop(global_env(Env)),
	Env60=[bv(u_parent, Parent)|Env],
	f_u_ob_c36_fcreate([quote, [u_cx]], Self_Init),
	LEnv=[bv(u_self, Self_Init)|Env60],
	get_var(LEnv, u_self, Self_Get),
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*], claz_base_character, "CX."),
				
				[ prog1,
				  [u_fixnum_c62_string, u_xx_next_cx_number_xx],
				  [u_increment_me, u_xx_next_cx_number_xx]
				]
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_add_unique_name(Self_Get, C62_symbol_Ret, Unique_name_Ret),
	f_u_cx_c63(u_parent, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_parent, Parent_Get),
	    get_var(LEnv, u_self, Self_Get14),
	    f_u_ob_c36_set(Self_Get14, u_parent, Parent_Get, C36_set_Ret),
	    get_var(LEnv, u_parent, Parent_Get16),
	    get_var(LEnv, u_self, Self_Get17),
	    f_u_cx_c36_add_child(Parent_Get16, Self_Get17, Add_child_Ret),
	    get_var(LEnv, u_parent, Parent_Get19),
	    get_var(LEnv, u_self, Self_Get18),
	    f_u_ob_c36_get(Parent_Get19,
			   u_mutations_tried_c63,
			   Mutations_tried_c63),
	    f_u_ob_c36_set(Self_Get18,
			   u_mutations_tried_c63,
			   Mutations_tried_c63,
			   C36_set_Ret78),
	    get_var(LEnv, u_parent, Parent_Get22),
	    f_u_ob_c36_get(Parent_Get22, ext_timeout, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(LEnv, u_parent, Parent_Get24),
		get_var(LEnv, u_self, Self_Get23),
		f_u_ob_c36_get(Parent_Get24, ext_timeout, Ext_timeout),
		-(Ext_timeout, 1, Ext_timeout64),
		f_u_ob_c36_set(Self_Get23,
			       ext_timeout,
			       Ext_timeout64,
			       TrueResult),
		_217191014=TrueResult
	    ;   _217191014=[]
	    ),
	    get_var(LEnv, u_parent, Parent_Get27),
	    get_var(LEnv, u_self, Self_Get26),
	    f_u_ob_c36_get(Parent_Get27, u_pseudo_sprout_c63, Pseudo_sprout_c63),
	    f_u_ob_c36_set(Self_Get26,
			   u_pseudo_sprout_c63,
			   Pseudo_sprout_c63,
			   C36_set_Ret79),
	    get_var(LEnv, u_parent, Parent_Get29),
	    get_var(LEnv, u_self, Self_Get28),
	    f_u_ob_c36_get(Parent_Get29, u_ancestors, Ancestors),
	    Ancestors67=[Parent_Get29|Ancestors],
	    f_u_ob_c36_set(Self_Get28, u_ancestors, Ancestors67, C36_set_Ret80),
	    get_var(LEnv, u_parent, Parent_Get32),
	    get_var(LEnv, u_self, Self_Get31),
	    f_u_ob_c36_get(Parent_Get32, u_all_obs, All_obs),
	    cl_copy_list(All_obs, All_obs69),
	    f_u_ob_c36_set(Self_Get31, u_all_obs, All_obs69, C36_set_Ret81),
	    get_var(LEnv, u_self, Self_Get33),
	    Lambda=closure([ClosureEnvironment|LEnv], LResult, [u_x],  (get_var(ClosureEnvironment, u_x, X_Get), cl_copy_list(X_Get, LResult))),
	    get_var(LEnv, u_parent, Parent_Get38),
	    f_u_ob_c36_get(Parent_Get38, u_type_hashing, Type_hashing),
	    cl_map(list, Lambda, Type_hashing, Type_hashing71),
	    f_u_ob_c36_set(Self_Get33,
			   u_type_hashing,
			   Type_hashing71,
			   C36_set_Ret82),
	    get_var(LEnv, u_parent, Parent_Get40),
	    get_var(LEnv, u_self, Self_Get39),
	    f_u_ob_c36_get(Parent_Get40, u_gen_switches, Gen_switches),
	    f_u_ob_c36_set(Self_Get39,
			   u_gen_switches,
			   Gen_switches,
			   C36_set_Ret83),
	    get_var(LEnv, u_parent, Parent_Get42),
	    get_var(LEnv, u_self, Self_Get41),
	    f_u_ob_c36_get(Parent_Get42, u_touched_facts, Touched_facts),
	    f_u_ob_c36_set(Self_Get41,
			   u_touched_facts,
			   Touched_facts,
			   TrueResult51),
	    ElseResult50=TrueResult51
	;   f_u_null_c63(u_parent, IFTEST43),
	    (   IFTEST43\==[]
	    ->  ElseResult50=[]
	    ;   get_var(LEnv, u_else, IFTEST45),
		(   IFTEST45\==[]
		->  cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					"cx$sprout: parent is not a context or NIL.")
			     ],
			     TrueResult48),
		    ElseResult50=TrueResult48
		;   ElseResult50=[]
		)
	    )
	),
	get_var(LEnv, u_parent, IFTEST53),
	(   IFTEST53\==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*], claz_base_character, "~A --> ~A~%"),
		       u_parent,
		       u_self
		     ],
		     TrueResult56),
	    _217480280=TrueResult56
	;   _217480280=[]
	),
	get_var(LEnv, u_self, Self_Get57),
	Self_Get57=FnResult.
:- set_opv(f_u_cx_c36_sprout, classof, claz_function),
   set_opv(u_cx_c36_sprout, compile_as, kw_function),
   set_opv(u_cx_c36_sprout, function, f_u_cx_c36_sprout),
   _Ignored4=u_cx_c36_sprout.
/*
:- side_effect(assert_lsp(u_cx_c36_sprout,
			  wl:lambda_def(defun, u_cx_c36_sprout, f_u_cx_c36_sprout, [u_parent], [[let, [[u_self, [u_ob_c36_fcreate, [quote, [u_cx]]]]], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "CX."), [prog1, [u_fixnum_c62_string, u_xx_next_cx_number_xx], [u_increment_me, u_xx_next_cx_number_xx]]]]], [cond, [[u_cx_c63, u_parent], [u_ob_c36_set, u_self, [quote, u_parent], u_parent], [u_cx_c36_add_child, u_parent, u_self], [u_ob_c36_set, u_self, [quote, u_mutations_tried_c63], [u_ob_c36_get, u_parent, [quote, u_mutations_tried_c63]]], [if, [u_ob_c36_get, u_parent, [quote, ext_timeout]], [u_ob_c36_set, u_self, [quote, ext_timeout], [-, [u_ob_c36_get, u_parent, [quote, ext_timeout]], 1]]], [u_ob_c36_set, u_self, [quote, u_pseudo_sprout_c63], [u_ob_c36_get, u_parent, [quote, u_pseudo_sprout_c63]]], [u_ob_c36_set, u_self, [quote, u_ancestors], [cons, u_parent, [u_ob_c36_get, u_parent, [quote, u_ancestors]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [copy_list, [u_ob_c36_get, u_parent, [quote, u_all_obs]]]], [u_ob_c36_set, u_self, [quote, u_type_hashing], [map, [quote, list], [lambda, [u_x], [copy_list, u_x]], [u_ob_c36_get, u_parent, [quote, u_type_hashing]]]], [u_ob_c36_set, u_self, [quote, u_gen_switches], [u_ob_c36_get, u_parent, [quote, u_gen_switches]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_ob_c36_get, u_parent, [quote, u_touched_facts]]]], [[u_null_c63, u_parent]], [u_else, [error, '$ARRAY'([*], claz_base_character, "cx$sprout: parent is not a context or NIL.")]]], [if, u_parent, [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "~A --> ~A~%"), u_parent, u_self]], u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_sprout,
			  wl:arglist_info(u_cx_c36_sprout, f_u_cx_c36_sprout, [u_parent], arginfo{all:[u_parent], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_parent], opt:0, req:[u_parent], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_sprout,
			  wl:init_args(exact_only, f_u_cx_c36_sprout))).
*/
/*
*/
/*
 Getters
*/
/*
*/
/*
(defun cx$parent (self)
  (ob$get self 'parent))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2046 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$parent',[self],['ob$get',self,[quote,parent]]])
wl:lambda_def(defun, u_cx_c36_parent, f_u_cx_c36_parent, [u_self], [[u_ob_c36_get, u_self, [quote, u_parent]]]).
wl:arglist_info(u_cx_c36_parent, f_u_cx_c36_parent, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_parent).

/*

### Compiled:  `U::CX$PARENT` 
*/
f_u_cx_c36_parent(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_parent, Parent),
	Parent=FnResult.
:- set_opv(f_u_cx_c36_parent, classof, claz_function),
   set_opv(u_cx_c36_parent, compile_as, kw_function),
   set_opv(u_cx_c36_parent, function, f_u_cx_c36_parent),
   _Ignored4=u_cx_c36_parent.
/*
:- side_effect(assert_lsp(u_cx_c36_parent,
			  wl:lambda_def(defun, u_cx_c36_parent, f_u_cx_c36_parent, [u_self], [[u_ob_c36_get, u_self, [quote, u_parent]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_parent,
			  wl:arglist_info(u_cx_c36_parent, f_u_cx_c36_parent, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_parent,
			  wl:init_args(exact_only, f_u_cx_c36_parent))).
*/
/*
(defun cx$ancestors (self)
  (ob$get self 'ancestors))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2096 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$ancestors',[self],['ob$get',self,[quote,ancestors]]])
wl:lambda_def(defun, u_cx_c36_ancestors, f_u_cx_c36_ancestors, [u_self], [[u_ob_c36_get, u_self, [quote, u_ancestors]]]).
wl:arglist_info(u_cx_c36_ancestors, f_u_cx_c36_ancestors, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_ancestors).

/*

### Compiled:  `U::CX$ANCESTORS` 
*/
f_u_cx_c36_ancestors(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_ancestors, Ancestors),
	Ancestors=FnResult.
:- set_opv(f_u_cx_c36_ancestors, classof, claz_function),
   set_opv(u_cx_c36_ancestors, compile_as, kw_function),
   set_opv(u_cx_c36_ancestors, function, f_u_cx_c36_ancestors),
   _Ignored4=u_cx_c36_ancestors.
/*
:- side_effect(assert_lsp(u_cx_c36_ancestors,
			  wl:lambda_def(defun, u_cx_c36_ancestors, f_u_cx_c36_ancestors, [u_self], [[u_ob_c36_get, u_self, [quote, u_ancestors]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_ancestors,
			  wl:arglist_info(u_cx_c36_ancestors, f_u_cx_c36_ancestors, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_ancestors,
			  wl:init_args(exact_only, f_u_cx_c36_ancestors))).
*/
/*
(defun cx$children (self)
  (ob$get self 'children))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2152 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$children',[self],['ob$get',self,[quote,children]]])
wl:lambda_def(defun, u_cx_c36_children, f_u_cx_c36_children, [u_self], [[u_ob_c36_get, u_self, [quote, u_children]]]).
wl:arglist_info(u_cx_c36_children, f_u_cx_c36_children, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_children).

/*

### Compiled:  `U::CX$CHILDREN` 
*/
f_u_cx_c36_children(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_children, Children),
	Children=FnResult.
:- set_opv(f_u_cx_c36_children, classof, claz_function),
   set_opv(u_cx_c36_children, compile_as, kw_function),
   set_opv(u_cx_c36_children, function, f_u_cx_c36_children),
   _Ignored4=u_cx_c36_children.
/*
:- side_effect(assert_lsp(u_cx_c36_children,
			  wl:lambda_def(defun, u_cx_c36_children, f_u_cx_c36_children, [u_self], [[u_ob_c36_get, u_self, [quote, u_children]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_children,
			  wl:arglist_info(u_cx_c36_children, f_u_cx_c36_children, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_children,
			  wl:init_args(exact_only, f_u_cx_c36_children))).
*/
/*
(defun cx$set-last-sprout-con (self val)
  (ndbg *gate-dbg* rule "setting last sprout concept = "(defun cx$set-last-sprout-con (self val)\n  (ndbg *gate-dbg* rule \"setting last sprout concept = ~A in ~A~%\" val self)\n  (if val\n      (let ((parent (ob$get self 'parent)))\n       (ob$set self 'last-sprout-con val)\n       (if (and parent\n                (null? (cx$last-sprout-con parent)))\n           (cx$set-last-sprout-con parent val)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2206 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$set-last-sprout-con',[self,val],[ndbg,'*gate-dbg*',rule,'$STRING'("setting last sprout concept = ~A in ~A~%"),val,self],[if,val,[let,[[parent,['ob$get',self,[quote,parent]]]],['ob$set',self,[quote,'last-sprout-con'],val],[if,[and,parent,['null?',['cx$last-sprout-con',parent]]],['cx$set-last-sprout-con',parent,val]]]]])
wl:lambda_def(defun, u_cx_c36_set_last_sprout_con, f_u_cx_c36_set_last_sprout_con, [u_self, u_val], [[u_ndbg, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "setting last sprout concept = ~A in ~A~%"), u_val, u_self], [if, u_val, [let, [[u_parent, [u_ob_c36_get, u_self, [quote, u_parent]]]], [u_ob_c36_set, u_self, [quote, u_last_sprout_con], u_val], [if, [and, u_parent, [u_null_c63, [u_cx_c36_last_sprout_con, u_parent]]], [u_cx_c36_set_last_sprout_con, u_parent, u_val]]]]]).
wl:arglist_info(u_cx_c36_set_last_sprout_con, f_u_cx_c36_set_last_sprout_con, [u_self, u_val], arginfo{all:[u_self, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_val], opt:0, req:[u_self, u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_set_last_sprout_con).

/*

### Compiled:  `U::CX$SET-LAST-SPROUT-CON` 
*/
f_u_cx_c36_set_last_sprout_con(Self, Val, LetResult) :-
	nop(global_env(Env)),
	Env29=[bv(u_self, Self), bv(u_val, Val)|Env],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_rule,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    "setting last sprout concept = ~A in ~A~%"),
		   u_val,
		   u_self
		 ],
		 Ndbg_Ret),
	get_var(Env29, u_val, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env29, u_self, Self_Get),
	    f_u_ob_c36_get(Self_Get, u_parent, Parent_Init),
	    LEnv=[bv(u_parent, Parent_Init)|Env29],
	    get_var(LEnv, u_self, Self_Get15),
	    get_var(LEnv, u_val, Val_Get16),
	    f_u_ob_c36_set(Self_Get15, u_last_sprout_con, Val_Get16, C36_set_Ret),
	    get_var(LEnv, u_parent, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_null_c63([u_cx_c36_last_sprout_con, u_parent], TrueResult),
		IFTEST17=TrueResult
	    ;   IFTEST17=[]
	    ),
	    (   IFTEST17\==[]
	    ->  get_var(LEnv, u_parent, Parent_Get23),
		get_var(LEnv, u_val, Val_Get24),
		f_u_cx_c36_set_last_sprout_con(Parent_Get23,
					       Val_Get24,
					       TrueResult25),
		LetResult=TrueResult25
	    ;   LetResult=[]
	    )
	;   LetResult=[]
	).
:- set_opv(f_u_cx_c36_set_last_sprout_con, classof, claz_function),
   set_opv(u_cx_c36_set_last_sprout_con, compile_as, kw_function),
   set_opv(u_cx_c36_set_last_sprout_con,
	   function,
	   f_u_cx_c36_set_last_sprout_con),
   _Ignored4=u_cx_c36_set_last_sprout_con.
/*
:- side_effect(assert_lsp(u_cx_c36_set_last_sprout_con,
			  wl:lambda_def(defun, u_cx_c36_set_last_sprout_con, f_u_cx_c36_set_last_sprout_con, [u_self, u_val], [[u_ndbg, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "setting last sprout concept = ~A in ~A~%"), u_val, u_self], [if, u_val, [let, [[u_parent, [u_ob_c36_get, u_self, [quote, u_parent]]]], [u_ob_c36_set, u_self, [quote, u_last_sprout_con], u_val], [if, [and, u_parent, [u_null_c63, [u_cx_c36_last_sprout_con, u_parent]]], [u_cx_c36_set_last_sprout_con, u_parent, u_val]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_set_last_sprout_con,
			  wl:arglist_info(u_cx_c36_set_last_sprout_con, f_u_cx_c36_set_last_sprout_con, [u_self, u_val], arginfo{all:[u_self, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_val], opt:0, req:[u_self, u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_set_last_sprout_con,
			  wl:init_args(exact_only, f_u_cx_c36_set_last_sprout_con))).
*/
/*
(defun cx$last-sprout-con (self)
  (ob$get self 'last-sprout-con))

; Pseudo sprouts don't inherit info from their parent (or ancestors).
; In particular, the top-context of an ob (an optimization) is no
; longer guaranteed to be an ancestor of a context in which that
; ob is asserted. In accordance, the top-context is never used for
; a pseudo-sprout context. Pseudo-sprouts are effectively root contexts
; but think they are sprouts of the specified context as far as &parent
; and &ancestor related calls go. By the way, all descendents of a
; pseudo sprout are pseudo sprouts.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$last-sprout-con',[self],['ob$get',self,[quote,'last-sprout-con']]])
wl:lambda_def(defun, u_cx_c36_last_sprout_con, f_u_cx_c36_last_sprout_con, [u_self], [[u_ob_c36_get, u_self, [quote, u_last_sprout_con]]]).
wl:arglist_info(u_cx_c36_last_sprout_con, f_u_cx_c36_last_sprout_con, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_last_sprout_con).

/*

### Compiled:  `U::CX$LAST-SPROUT-CON` 
*/
f_u_cx_c36_last_sprout_con(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_last_sprout_con, Last_sprout_con),
	Last_sprout_con=FnResult.
:- set_opv(f_u_cx_c36_last_sprout_con, classof, claz_function),
   set_opv(u_cx_c36_last_sprout_con, compile_as, kw_function),
   set_opv(u_cx_c36_last_sprout_con, function, f_u_cx_c36_last_sprout_con),
   _Ignored4=u_cx_c36_last_sprout_con.
/*
:- side_effect(assert_lsp(u_cx_c36_last_sprout_con,
			  wl:lambda_def(defun, u_cx_c36_last_sprout_con, f_u_cx_c36_last_sprout_con, [u_self], [[u_ob_c36_get, u_self, [quote, u_last_sprout_con]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_last_sprout_con,
			  wl:arglist_info(u_cx_c36_last_sprout_con, f_u_cx_c36_last_sprout_con, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_last_sprout_con,
			  wl:init_args(exact_only, f_u_cx_c36_last_sprout_con))).
*/
/*
 Pseudo sprouts don't inherit info from their parent (or ancestors).
*/
/*
 In particular, the top-context of an ob (an optimization) is no
*/
/*
 longer guaranteed to be an ancestor of a context in which that
*/
/*
 ob is asserted. In accordance, the top-context is never used for
*/
/*
 a pseudo-sprout context. Pseudo-sprouts are effectively root contexts
*/
/*
 but think they are sprouts of the specified context as far as &parent
*/
/*
 and &ancestor related calls go. By the way, all descendents of a
*/
/*
 pseudo sprout are pseudo sprouts.
*/
/*
(defun cx$pseudo-sprout-of (self context)
  (if (ob$get self 'parent)
      (error "Cannot make a pseudo sprout out of a context with a parent")
      (progn
       (ob$set self 'parent context)
       (ob$set self 'ancestors (cons context (ob$get context 'ancestors)))
       (cx$add-child context self)
       (ob$set self 'pseudo-sprout? t))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3131 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$pseudo-sprout-of',[self,context],[if,['ob$get',self,[quote,parent]],[error,'$STRING'("Cannot make a pseudo sprout out of a context with a parent")],[progn,['ob$set',self,[quote,parent],context],['ob$set',self,[quote,ancestors],[cons,context,['ob$get',context,[quote,ancestors]]]],['cx$add-child',context,self],['ob$set',self,[quote,'pseudo-sprout?'],t]]]])
wl:lambda_def(defun, u_cx_c36_pseudo_sprout_of, f_u_cx_c36_pseudo_sprout_of, [u_self, u_context], [[if, [u_ob_c36_get, u_self, [quote, u_parent]], [error, '$ARRAY'([*], claz_base_character, "Cannot make a pseudo sprout out of a context with a parent")], [progn, [u_ob_c36_set, u_self, [quote, u_parent], u_context], [u_ob_c36_set, u_self, [quote, u_ancestors], [cons, u_context, [u_ob_c36_get, u_context, [quote, u_ancestors]]]], [u_cx_c36_add_child, u_context, u_self], [u_ob_c36_set, u_self, [quote, u_pseudo_sprout_c63], t]]]]).
wl:arglist_info(u_cx_c36_pseudo_sprout_of, f_u_cx_c36_pseudo_sprout_of, [u_self, u_context], arginfo{all:[u_self, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_context], opt:0, req:[u_self, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_pseudo_sprout_of).

/*

### Compiled:  `U::CX$PSEUDO-SPROUT-OF` 
*/
f_u_cx_c36_pseudo_sprout_of(Self, Context, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_self, Self), bv(u_context, Context)|Env],
	get_var(Env22, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_parent, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"Cannot make a pseudo sprout out of a context with a parent")
		     ],
		     TrueResult),
	    FnResult=TrueResult
	;   get_var(Env22, u_context, Context_Get),
	    get_var(Env22, u_self, Self_Get10),
	    f_u_ob_c36_set(Self_Get10, u_parent, Context_Get, C36_set_Ret),
	    get_var(Env22, u_context, Context_Get13),
	    get_var(Env22, u_self, Self_Get12),
	    f_u_ob_c36_get(Context_Get13, u_ancestors, Ancestors),
	    Ancestors26=[Context_Get13|Ancestors],
	    f_u_ob_c36_set(Self_Get12, u_ancestors, Ancestors26, C36_set_Ret28),
	    get_var(Env22, u_context, Context_Get15),
	    get_var(Env22, u_self, Self_Get16),
	    f_u_cx_c36_add_child(Context_Get15, Self_Get16, Add_child_Ret),
	    get_var(Env22, u_self, Self_Get17),
	    f_u_ob_c36_set(Self_Get17, u_pseudo_sprout_c63, t, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_pseudo_sprout_of, classof, claz_function),
   set_opv(u_cx_c36_pseudo_sprout_of, compile_as, kw_function),
   set_opv(u_cx_c36_pseudo_sprout_of, function, f_u_cx_c36_pseudo_sprout_of),
   _Ignored4=u_cx_c36_pseudo_sprout_of.
/*
:- side_effect(assert_lsp(u_cx_c36_pseudo_sprout_of,
			  wl:lambda_def(defun, u_cx_c36_pseudo_sprout_of, f_u_cx_c36_pseudo_sprout_of, [u_self, u_context], [[if, [u_ob_c36_get, u_self, [quote, u_parent]], [error, '$ARRAY'([*], claz_base_character, "Cannot make a pseudo sprout out of a context with a parent")], [progn, [u_ob_c36_set, u_self, [quote, u_parent], u_context], [u_ob_c36_set, u_self, [quote, u_ancestors], [cons, u_context, [u_ob_c36_get, u_context, [quote, u_ancestors]]]], [u_cx_c36_add_child, u_context, u_self], [u_ob_c36_set, u_self, [quote, u_pseudo_sprout_c63], t]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_pseudo_sprout_of,
			  wl:arglist_info(u_cx_c36_pseudo_sprout_of, f_u_cx_c36_pseudo_sprout_of, [u_self, u_context], arginfo{all:[u_self, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_context], opt:0, req:[u_self, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_pseudo_sprout_of,
			  wl:init_args(exact_only, f_u_cx_c36_pseudo_sprout_of))).
*/
/*
(defun cx$touch-fact (self fact)
  (if (and (touchable-fact? fact)
           (not (memq? fact (ob$get self 'touched-facts))))
      (ob$set self 'touched-facts (cons fact (ob$get self 'touched-facts)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3479 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$touch-fact',[self,fact],[if,[and,['touchable-fact?',fact],[not,['memq?',fact,['ob$get',self,[quote,'touched-facts']]]]],['ob$set',self,[quote,'touched-facts'],[cons,fact,['ob$get',self,[quote,'touched-facts']]]]]])
wl:lambda_def(defun, u_cx_c36_touch_fact, f_u_cx_c36_touch_fact, [u_self, u_fact], [[if, [and, [u_touchable_fact_c63, u_fact], [not, [u_memq_c63, u_fact, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [cons, u_fact, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]]]).
wl:arglist_info(u_cx_c36_touch_fact, f_u_cx_c36_touch_fact, [u_self, u_fact], arginfo{all:[u_self, u_fact], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_fact], opt:0, req:[u_self, u_fact], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_touch_fact).

/*

### Compiled:  `U::CX$TOUCH-FACT` 
*/
f_u_cx_c36_touch_fact(Self, Fact, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_self, Self), bv(u_fact, Fact)|Env],
	f_u_touchable_fact_c63(u_fact, IFTEST9),
	(   IFTEST9\==[]
	->  f_u_memq_c63(u_fact,
			 [u_ob_c36_get, u_self, [quote, u_touched_facts]],
			 Not_Param),
	    cl_not(Not_Param, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env18, u_fact, Fact_Get),
	    get_var(Env18, u_self, Self_Get14),
	    f_u_ob_c36_get(Self_Get14, u_touched_facts, Touched_facts),
	    Touched_facts22=[Fact_Get|Touched_facts],
	    f_u_ob_c36_set(Self_Get14,
			   u_touched_facts,
			   Touched_facts22,
			   TrueResult15),
	    FnResult=TrueResult15
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_touch_fact, classof, claz_function),
   set_opv(u_cx_c36_touch_fact, compile_as, kw_function),
   set_opv(u_cx_c36_touch_fact, function, f_u_cx_c36_touch_fact),
   _Ignored4=u_cx_c36_touch_fact.
/*
:- side_effect(assert_lsp(u_cx_c36_touch_fact,
			  wl:lambda_def(defun, u_cx_c36_touch_fact, f_u_cx_c36_touch_fact, [u_self, u_fact], [[if, [and, [u_touchable_fact_c63, u_fact], [not, [u_memq_c63, u_fact, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [cons, u_fact, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_touch_fact,
			  wl:arglist_info(u_cx_c36_touch_fact, f_u_cx_c36_touch_fact, [u_self, u_fact], arginfo{all:[u_self, u_fact], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_fact], opt:0, req:[u_self, u_fact], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_touch_fact,
			  wl:init_args(exact_only, f_u_cx_c36_touch_fact))).
*/
/*
(defun cx$sorted-all-obs (self)
  (if (ob$get self 'pseudo-sprout?)
      (progn
       (format *gate-output* "GATE bug warning: Can't sort a pseudo-sprout"(defun cx$sorted-all-obs (self)\n  (if (ob$get self 'pseudo-sprout?)\n      (progn\n       (format *gate-output* \"GATE bug warning: Can't sort a pseudo-sprout~%\")\n       (ob$get self 'all-obs))\n      (yloop (initial (result nil)\n                      (rest (ob$get self 'all-obs)))\n             (yfor context in (cons self (ob$get self 'ancestors)))\n             (ydo (yloop (yfor ob in rest)\n                         (ydo (if (memq? context (ob$gets ob 'top-context))\n                                  (progn\n                                   (setq result (cons ob result))\n                                   (setq rest (delq! ob rest)))))))\n             (yresult result))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3685 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$sorted-all-obs',[self],[if,['ob$get',self,[quote,'pseudo-sprout?']],[progn,[format,'*gate-output*','$STRING'("GATE bug warning: Can't sort a pseudo-sprout~%")],['ob$get',self,[quote,'all-obs']]],[yloop,[initial,[result,[]],[rest,['ob$get',self,[quote,'all-obs']]]],[yfor,context,in,[cons,self,['ob$get',self,[quote,ancestors]]]],[ydo,[yloop,[yfor,ob,in,rest],[ydo,[if,['memq?',context,['ob$gets',ob,[quote,'top-context']]],[progn,[setq,result,[cons,ob,result]],[setq,rest,['delq!',ob,rest]]]]]]],[yresult,result]]]])
wl:lambda_def(defun, u_cx_c36_sorted_all_obs, f_u_cx_c36_sorted_all_obs, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "GATE bug warning: Can't sort a pseudo-sprout~%")], [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_yloop, [u_initial, [u_result, []], [rest, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_yfor, u_context, u_in, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]], [u_ydo, [u_yloop, [u_yfor, u_ob, u_in, rest], [u_ydo, [if, [u_memq_c63, u_context, [u_ob_c36_gets, u_ob, [quote, u_top_context]]], [progn, [setq, u_result, [cons, u_ob, u_result]], [setq, rest, [u_delq_c33, u_ob, rest]]]]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_cx_c36_sorted_all_obs, f_u_cx_c36_sorted_all_obs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_sorted_all_obs).

/*

### Compiled:  `U::CX$SORTED-ALL-OBS` 
*/
f_u_cx_c36_sorted_all_obs(Self, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_self, Self)|Env],
	get_var(Env16, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_pseudo_sprout_c63, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env16, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 "GATE bug warning: Can't sort a pseudo-sprout~%")
		      ],
		      Format_Ret),
	    get_var(Env16, u_self, Self_Get11),
	    f_u_ob_c36_get(Self_Get11, u_all_obs, TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [u_result, []],
			  [rest, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
			],
			
			[ u_yfor,
			  u_context,
			  u_in,
			  
			  [ cons,
			    u_self,
			    [u_ob_c36_get, u_self, [quote, u_ancestors]]
			  ]
			],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    [u_yfor, u_ob, u_in, rest],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ u_memq_c63,
				  u_context,
				  [u_ob_c36_gets, u_ob, [quote, u_top_context]]
				],
				
				[ progn,
				  [setq, u_result, [cons, u_ob, u_result]],
				  [setq, rest, [u_delq_c33, u_ob, rest]]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_sorted_all_obs, classof, claz_function),
   set_opv(u_cx_c36_sorted_all_obs, compile_as, kw_function),
   set_opv(u_cx_c36_sorted_all_obs, function, f_u_cx_c36_sorted_all_obs),
   _Ignored4=u_cx_c36_sorted_all_obs.
/*
:- side_effect(assert_lsp(u_cx_c36_sorted_all_obs,
			  wl:lambda_def(defun, u_cx_c36_sorted_all_obs, f_u_cx_c36_sorted_all_obs, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "GATE bug warning: Can't sort a pseudo-sprout~%")], [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_yloop, [u_initial, [u_result, []], [rest, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_yfor, u_context, u_in, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]], [u_ydo, [u_yloop, [u_yfor, u_ob, u_in, rest], [u_ydo, [if, [u_memq_c63, u_context, [u_ob_c36_gets, u_ob, [quote, u_top_context]]], [progn, [setq, u_result, [cons, u_ob, u_result]], [setq, rest, [u_delq_c33, u_ob, rest]]]]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_sorted_all_obs,
			  wl:arglist_info(u_cx_c36_sorted_all_obs, f_u_cx_c36_sorted_all_obs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_sorted_all_obs,
			  wl:init_args(exact_only, f_u_cx_c36_sorted_all_obs))).
*/
/*
(defun cx$add-child (self child)
  (ob$set self 'children (cons child (ob$get self 'children))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4360 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$add-child',[self,child],['ob$set',self,[quote,children],[cons,child,['ob$get',self,[quote,children]]]]])
wl:lambda_def(defun, u_cx_c36_add_child, f_u_cx_c36_add_child, [u_self, u_child], [[u_ob_c36_set, u_self, [quote, u_children], [cons, u_child, [u_ob_c36_get, u_self, [quote, u_children]]]]]).
wl:arglist_info(u_cx_c36_add_child, f_u_cx_c36_add_child, [u_self, u_child], arginfo{all:[u_self, u_child], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_child], opt:0, req:[u_self, u_child], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_add_child).

/*

### Compiled:  `U::CX$ADD-CHILD` 
*/
f_u_cx_c36_add_child(Self, Child, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self), bv(u_child, Child)|Env],
	get_var(Env12, u_child, Child_Get),
	get_var(Env12, u_self, Self_Get9),
	f_u_ob_c36_get(Self_Get9, u_children, Children),
	Children16=[Child_Get|Children],
	f_u_ob_c36_set(Self_Get9, u_children, Children16, C36_set_Ret),
	C36_set_Ret=FnResult.
:- set_opv(f_u_cx_c36_add_child, classof, claz_function),
   set_opv(u_cx_c36_add_child, compile_as, kw_function),
   set_opv(u_cx_c36_add_child, function, f_u_cx_c36_add_child),
   _Ignored4=u_cx_c36_add_child.
/*
:- side_effect(assert_lsp(u_cx_c36_add_child,
			  wl:lambda_def(defun, u_cx_c36_add_child, f_u_cx_c36_add_child, [u_self, u_child], [[u_ob_c36_set, u_self, [quote, u_children], [cons, u_child, [u_ob_c36_get, u_self, [quote, u_children]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_add_child,
			  wl:arglist_info(u_cx_c36_add_child, f_u_cx_c36_add_child, [u_self, u_child], arginfo{all:[u_self, u_child], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_child], opt:0, req:[u_self, u_child], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_add_child,
			  wl:init_args(exact_only, f_u_cx_c36_add_child))).
*/
/*
(defun cx$most-recent-child (self)
  (if (ob$get self 'children)
      (car (ob$get self 'children))
      nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4458 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$most-recent-child',[self],[if,['ob$get',self,[quote,children]],[car,['ob$get',self,[quote,children]]],[]]])
wl:lambda_def(defun, u_cx_c36_most_recent_child, f_u_cx_c36_most_recent_child, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_children]], [car, [u_ob_c36_get, u_self, [quote, u_children]]], []]]).
wl:arglist_info(u_cx_c36_most_recent_child, f_u_cx_c36_most_recent_child, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_most_recent_child).

/*

### Compiled:  `U::CX$MOST-RECENT-CHILD` 
*/
f_u_cx_c36_most_recent_child(Self, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self)|Env],
	get_var(Env14, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_children, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_self, Self_Get10),
	    f_u_ob_c36_get(Self_Get10, u_children, Children),
	    cl_car(Children, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_most_recent_child, classof, claz_function),
   set_opv(u_cx_c36_most_recent_child, compile_as, kw_function),
   set_opv(u_cx_c36_most_recent_child, function, f_u_cx_c36_most_recent_child),
   _Ignored4=u_cx_c36_most_recent_child.
/*
:- side_effect(assert_lsp(u_cx_c36_most_recent_child,
			  wl:lambda_def(defun, u_cx_c36_most_recent_child, f_u_cx_c36_most_recent_child, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_children]], [car, [u_ob_c36_get, u_self, [quote, u_children]]], []]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_most_recent_child,
			  wl:arglist_info(u_cx_c36_most_recent_child, f_u_cx_c36_most_recent_child, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_most_recent_child,
			  wl:init_args(exact_only, f_u_cx_c36_most_recent_child))).
*/
/*
(defun cx$leaf-descendants (self)
  (if (null? (ob$get self 'children))
      (list self)
      (yloop (initial (result nil))
             (yfor child in (ob$get self 'children))
             (ydo (setq result (append result (cx$leaf-descendants child))))
             (yresult result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4572 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$leaf-descendants',[self],[if,['null?',['ob$get',self,[quote,children]]],[list,self],[yloop,[initial,[result,[]]],[yfor,child,in,['ob$get',self,[quote,children]]],[ydo,[setq,result,[append,result,['cx$leaf-descendants',child]]]],[yresult,result]]]])
wl:lambda_def(defun, u_cx_c36_leaf_descendants, f_u_cx_c36_leaf_descendants, [u_self], [[if, [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]], [list, u_self], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_child, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [setq, u_result, [append, u_result, [u_cx_c36_leaf_descendants, u_child]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_cx_c36_leaf_descendants, f_u_cx_c36_leaf_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_leaf_descendants).

/*

### Compiled:  `U::CX$LEAF-DESCENDANTS` 
*/
f_u_cx_c36_leaf_descendants(Self, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self)|Env],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_self, Self_Get),
	    FnResult=[Self_Get]
	;   f_u_yloop(
		      [ [u_initial, [u_result, []]],
			
			[ u_yfor,
			  u_child,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_children]]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ append,
			      u_result,
			      [u_cx_c36_leaf_descendants, u_child]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_leaf_descendants, classof, claz_function),
   set_opv(u_cx_c36_leaf_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_leaf_descendants, function, f_u_cx_c36_leaf_descendants),
   _Ignored4=u_cx_c36_leaf_descendants.
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_descendants,
			  wl:lambda_def(defun, u_cx_c36_leaf_descendants, f_u_cx_c36_leaf_descendants, [u_self], [[if, [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]], [list, u_self], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_child, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [setq, u_result, [append, u_result, [u_cx_c36_leaf_descendants, u_child]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_descendants,
			  wl:arglist_info(u_cx_c36_leaf_descendants, f_u_cx_c36_leaf_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_descendants,
			  wl:init_args(exact_only, f_u_cx_c36_leaf_descendants))).
*/
/*
(defun cx$descendants (self)
  (if (null? (ob$get self 'children))
      (list self)
      (yloop (initial (result (list self)))
             (yfor child in (ob$get self 'children))
             (ydo (setq result (append result (cx$descendants child))))
             (yresult result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4862 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$descendants',[self],[if,['null?',['ob$get',self,[quote,children]]],[list,self],[yloop,[initial,[result,[list,self]]],[yfor,child,in,['ob$get',self,[quote,children]]],[ydo,[setq,result,[append,result,['cx$descendants',child]]]],[yresult,result]]]])
wl:lambda_def(defun, u_cx_c36_descendants, f_u_cx_c36_descendants, [u_self], [[if, [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]], [list, u_self], [u_yloop, [u_initial, [u_result, [list, u_self]]], [u_yfor, u_child, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [setq, u_result, [append, u_result, [u_cx_c36_descendants, u_child]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_cx_c36_descendants, f_u_cx_c36_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_descendants).

/*

### Compiled:  `U::CX$DESCENDANTS` 
*/
f_u_cx_c36_descendants(Self, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self)|Env],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_self, Self_Get),
	    FnResult=[Self_Get]
	;   f_u_yloop(
		      [ [u_initial, [u_result, [list, u_self]]],
			
			[ u_yfor,
			  u_child,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_children]]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    [append, u_result, [u_cx_c36_descendants, u_child]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_descendants, classof, claz_function),
   set_opv(u_cx_c36_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_descendants, function, f_u_cx_c36_descendants),
   _Ignored4=u_cx_c36_descendants.
/*
:- side_effect(assert_lsp(u_cx_c36_descendants,
			  wl:lambda_def(defun, u_cx_c36_descendants, f_u_cx_c36_descendants, [u_self], [[if, [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]], [list, u_self], [u_yloop, [u_initial, [u_result, [list, u_self]]], [u_yfor, u_child, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [setq, u_result, [append, u_result, [u_cx_c36_descendants, u_child]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_descendants,
			  wl:arglist_info(u_cx_c36_descendants, f_u_cx_c36_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_descendants,
			  wl:init_args(exact_only, f_u_cx_c36_descendants))).
*/
/*
(defun cx$tree-print (self)
  (map 'list (lambda (x) (cx$print x))
        (cx$descendants self)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5150 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$tree-print',[self],[map,[quote,list],[lambda,[x],['cx$print',x]],['cx$descendants',self]]])
wl:lambda_def(defun, u_cx_c36_tree_print, f_u_cx_c36_tree_print, [u_self], [[map, [quote, list], [lambda, [u_x], [u_cx_c36_print, u_x]], [u_cx_c36_descendants, u_self]]]).
wl:arglist_info(u_cx_c36_tree_print, f_u_cx_c36_tree_print, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_tree_print).

/*

### Compiled:  `U::CX$TREE-PRINT` 
*/
f_u_cx_c36_tree_print(Self, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self)|Env],
	Lambda=closure([ClosureEnvironment|Env14], LResult, [u_x],  (get_var(ClosureEnvironment, u_x, X_Get), f_u_cx_c36_print(X_Get, LResult))),
	get_var(Env14, u_self, Self_Get),
	f_u_cx_c36_descendants(Self_Get, C36_descendants_Ret),
	cl_map(list, Lambda, C36_descendants_Ret, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_cx_c36_tree_print, classof, claz_function),
   set_opv(u_cx_c36_tree_print, compile_as, kw_function),
   set_opv(u_cx_c36_tree_print, function, f_u_cx_c36_tree_print),
   _Ignored4=u_cx_c36_tree_print.
/*
:- side_effect(assert_lsp(u_cx_c36_tree_print,
			  wl:lambda_def(defun, u_cx_c36_tree_print, f_u_cx_c36_tree_print, [u_self], [[map, [quote, list], [lambda, [u_x], [u_cx_c36_print, u_x]], [u_cx_c36_descendants, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_tree_print,
			  wl:arglist_info(u_cx_c36_tree_print, f_u_cx_c36_tree_print, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_tree_print,
			  wl:init_args(exact_only, f_u_cx_c36_tree_print))).
*/
/*
(defun cx$root (self)
  (if (ob$get self 'ancestors)
      (tlast (ob$get self 'ancestors))
      self))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5250 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$root',[self],[if,['ob$get',self,[quote,ancestors]],[tlast,['ob$get',self,[quote,ancestors]]],self]])
wl:lambda_def(defun, u_cx_c36_root, f_u_cx_c36_root, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_ancestors]], [u_tlast, [u_ob_c36_get, u_self, [quote, u_ancestors]]], u_self]]).
wl:arglist_info(u_cx_c36_root, f_u_cx_c36_root, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_root).

/*

### Compiled:  `U::CX$ROOT` 
*/
f_u_cx_c36_root(Self, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_self, Self)|Env],
	get_var(Env15, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_ancestors, IFTEST),
	(   IFTEST\==[]
	->  f_u_tlast([u_ob_c36_get, u_self, [quote, u_ancestors]], TrueResult),
	    FnResult=TrueResult
	;   get_var(Env15, u_self, Self_Get10),
	    FnResult=Self_Get10
	).
:- set_opv(f_u_cx_c36_root, classof, claz_function),
   set_opv(u_cx_c36_root, compile_as, kw_function),
   set_opv(u_cx_c36_root, function, f_u_cx_c36_root),
   _Ignored4=u_cx_c36_root.
/*
:- side_effect(assert_lsp(u_cx_c36_root,
			  wl:lambda_def(defun, u_cx_c36_root, f_u_cx_c36_root, [u_self], [[if, [u_ob_c36_get, u_self, [quote, u_ancestors]], [u_tlast, [u_ob_c36_get, u_self, [quote, u_ancestors]]], u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_root,
			  wl:arglist_info(u_cx_c36_root, f_u_cx_c36_root, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_root,
			  wl:init_args(exact_only, f_u_cx_c36_root))).
*/
/*
(setq *disallow-non-leaf?* nil)
; Note: The above should be set to t if you are using truth
; maintenance!

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5356 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*disallow-non-leaf?*',[]])
:- set_var(AEnv, setq, u_xx_disallow_non_leaf_c63_xx, []).
/*
 Note: The above should be set to t if you are using truth
*/
/*
 maintenance!
*/
/*
(setq *hashing?* t)
; Hashing flag is global, not on a context-by-context basis. So,
; it is recommended not to change this flag during a session.
; Actually, you will be OK if after you change it, you never access
; any contexts used before the change.

; RESTRICTION if you are using hashing:
; Asserted obs cannot change type. If you want to change the type of
; an ob, first retract it from all the contexts in which it is
; asserted, change the type, then reassert it in all the contexts.

; Otherwise, hashing is recommended for faster operation.
; Typical speedups (in min:sec)
; 5:27 no hashing (almost all compiled)
; 3:31 hashing with no use of cx$get-all-ty (almost all compiled)
; 3:12 hashing with use of cx$get-all-ty (almost all compiled)
; 3:02 hashing with use of cx$get-all-ty (all compiled)
; 4:05 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
; 3:39 hashing with use of cx$get-all-ty and no unify reverse
;      and add slot reversal (all compiled but add-slot-val)
; 3:06 hashing with use of cx$get-all-ty and no unify reverse
;      and add slot reversal (all compiled)
; --- below not run in fresh GATE (garbage and other factors will cause
;     increase)
; 5:10 no hashing with use of cx$get-all-ty (all compiled)
; 3:54 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
; 5:22 no hashing with use of cx$get-all-ty and no unify reverse (all compiled)

; Very strange: why is it apparently slower with no unify reverse??!!!
; Because more predictive slots (e.g., type) are last.
; Changed add-slot-value to prepend.

; We could also hash on the type of the OBJ, and other common ob
; attributes.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*hashing?*',t])
:- set_var(AEnv, setq, u_xx_hashing_c63_xx, t).
/*
 Hashing flag is global, not on a context-by-context basis. So,
*/
/*
 it is recommended not to change this flag during a session.
*/
/*
 Actually, you will be OK if after you change it, you never access
*/
/*
 any contexts used before the change.
*/
/*
 RESTRICTION if you are using hashing:
*/
/*
 Asserted obs cannot change type. If you want to change the type of
*/
/*
 an ob, first retract it from all the contexts in which it is
*/
/*
 asserted, change the type, then reassert it in all the contexts.
*/
/*
 Otherwise, hashing is recommended for faster operation.
*/
/*
 Typical speedups (in min:sec)
*/
/*
 5:27 no hashing (almost all compiled)
*/
/*
 3:31 hashing with no use of cx$get-all-ty (almost all compiled)
*/
/*
 3:12 hashing with use of cx$get-all-ty (almost all compiled)
*/
/*
 3:02 hashing with use of cx$get-all-ty (all compiled)
*/
/*
 4:05 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
*/
/*
 3:39 hashing with use of cx$get-all-ty and no unify reverse
*/
/*
      and add slot reversal (all compiled but add-slot-val)
*/
/*
 3:06 hashing with use of cx$get-all-ty and no unify reverse
*/
/*
      and add slot reversal (all compiled)
*/
/*
 --- below not run in fresh GATE (garbage and other factors will cause
*/
/*
     increase)
*/
/*
 5:10 no hashing with use of cx$get-all-ty (all compiled)
*/
/*
 3:54 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
*/
/*
 5:22 no hashing with use of cx$get-all-ty and no unify reverse (all compiled)
*/
/*
 Very strange: why is it apparently slower with no unify reverse??!!!
*/
/*
 Because more predictive slots (e.g., type) are last.
*/
/*
 Changed add-slot-value to prepend.
*/
/*
 We could also hash on the type of the OBJ, and other common ob
*/
/*
 attributes.
*/
/*
(defun cx$assert-hash (self ob)
  (let ((type (ob$ty ob)))
    (if (ty? type)
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (setf (cdr found) (cons ob (cdr found)))
              (ob$set self 'type-hashing
                      (cons (list type ob) (ob$get self 'type-hashing))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7129 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$assert-hash',[self,ob],[let,[[type,['ob$ty',ob]]],[if,['ty?',type],[let,[[found,[assq,type,['ob$get',self,[quote,'type-hashing']]]]],[if,found,[setf,[cdr,found],[cons,ob,[cdr,found]]],['ob$set',self,[quote,'type-hashing'],[cons,[list,type,ob],['ob$get',self,[quote,'type-hashing']]]]]]]]])
wl:lambda_def(defun, u_cx_c36_assert_hash, f_u_cx_c36_assert_hash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [u_ty_c63, type], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [setf, [cdr, u_found], [cons, u_ob, [cdr, u_found]]], [u_ob_c36_set, u_self, [quote, u_type_hashing], [cons, [list, type, u_ob], [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]]]]]]).
wl:arglist_info(u_cx_c36_assert_hash, f_u_cx_c36_assert_hash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_assert_hash).

/*

### Compiled:  `U::CX$ASSERT-HASH` 
*/
f_u_cx_c36_assert_hash(Self, Ob, LetResult14) :-
	nop(global_env(Env)),
	Env32=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[bv(type, Type_Init)|Env32],
	f_u_ty_c63(type, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    LEnv15=[bv(u_found, Found_Init)|LEnv],
	    get_var(LEnv15, u_found, IFTEST17),
	    (   IFTEST17\==[]
	    ->  get_var(LEnv15, u_found, Found_Get20),
		get_var(LEnv15, u_ob, Ob_Get),
		cl_cdr(Found_Get20, Cdr_Ret),
		_235668402=[Ob_Get|Cdr_Ret],
		cl_rplacd(Found_Get20, _235668402, TrueResult),
		LetResult14=TrueResult
	    ;   get_var(LEnv15, type, Type_Get),
		get_var(LEnv15, u_ob, Ob_Get25),
		get_var(LEnv15, u_self, Self_Get),
		CAR=[Type_Get, Ob_Get25],
		get_var(LEnv15, u_self, Self_Get26),
		f_u_ob_c36_get(Self_Get26, u_type_hashing, Type_hashing),
		Type_hashing36=[CAR|Type_hashing],
		f_u_ob_c36_set(Self_Get,
			       u_type_hashing,
			       Type_hashing36,
			       ElseResult),
		LetResult14=ElseResult
	    )
	;   LetResult14=[]
	).
:- set_opv(f_u_cx_c36_assert_hash, classof, claz_function),
   set_opv(u_cx_c36_assert_hash, compile_as, kw_function),
   set_opv(u_cx_c36_assert_hash, function, f_u_cx_c36_assert_hash),
   _Ignored4=u_cx_c36_assert_hash.
/*
:- side_effect(assert_lsp(u_cx_c36_assert_hash,
			  wl:lambda_def(defun, u_cx_c36_assert_hash, f_u_cx_c36_assert_hash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [u_ty_c63, type], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [setf, [cdr, u_found], [cons, u_ob, [cdr, u_found]]], [u_ob_c36_set, u_self, [quote, u_type_hashing], [cons, [list, type, u_ob], [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_assert_hash,
			  wl:arglist_info(u_cx_c36_assert_hash, f_u_cx_c36_assert_hash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_assert_hash,
			  wl:init_args(exact_only, f_u_cx_c36_assert_hash))).
*/
/*
(defun cx$retract-unhash (self ob)
  (let ((type (ob$ty ob)))
    (if (ty? type)
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (progn
               (if (not (memq? ob (cdr found)))
                   (error "cx$retract-unhash: I can't unhash "(defun cx$retract-unhash (self ob)\n  (let ((type (ob$ty ob)))\n    (if (ty? type)\n        (let ((found (assq type (ob$get self 'type-hashing))))\n          (if found\n              (progn\n               (if (not (memq? ob (cdr found)))\n                   (error \"cx$retract-unhash: I can't unhash ~A\" ob))\n               (setf (cdr found) (delq! ob (cdr found))))\n              (error \"cx$retract-unhash: Strange, I can't seem to unhash ~A\"\n                     ob))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7465 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retract-unhash',[self,ob],[let,[[type,['ob$ty',ob]]],[if,['ty?',type],[let,[[found,[assq,type,['ob$get',self,[quote,'type-hashing']]]]],[if,found,[progn,[if,[not,['memq?',ob,[cdr,found]]],[error,'$STRING'("cx$retract-unhash: I can't unhash ~A"),ob]],[setf,[cdr,found],['delq!',ob,[cdr,found]]]],[error,'$STRING'("cx$retract-unhash: Strange, I can't seem to unhash ~A"),ob]]]]]])
wl:lambda_def(defun, u_cx_c36_retract_unhash, f_u_cx_c36_retract_unhash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [u_ty_c63, type], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [progn, [if, [not, [u_memq_c63, u_ob, [cdr, u_found]]], [error, '$ARRAY'([*], claz_base_character, "cx$retract-unhash: I can't unhash ~A"), u_ob]], [setf, [cdr, u_found], [u_delq_c33, u_ob, [cdr, u_found]]]], [error, '$ARRAY'([*], claz_base_character, "cx$retract-unhash: Strange, I can't seem to unhash ~A"), u_ob]]]]]]).
wl:arglist_info(u_cx_c36_retract_unhash, f_u_cx_c36_retract_unhash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retract_unhash).

/*

### Compiled:  `U::CX$RETRACT-UNHASH` 
*/
f_u_cx_c36_retract_unhash(Self, Ob, LetResult14) :-
	nop(global_env(Env)),
	Env32=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[bv(type, Type_Init)|Env32],
	f_u_ty_c63(type, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    LEnv15=[bv(u_found, Found_Init)|LEnv],
	    get_var(LEnv15, u_found, IFTEST17),
	    (   IFTEST17\==[]
	    ->  f_u_memq_c63(u_ob, [cdr, u_found], PredArgResult),
		(   PredArgResult==[]
		->  get_var(LEnv15, u_ob, Ob_Get),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					"cx$retract-unhash: I can't unhash ~A"),
			       Ob_Get
			     ],
			     TrueResult),
		    _237360724=TrueResult
		;   _237360724=[]
		),
		get_var(LEnv15, u_found, Found_Get25),
		f_u_delq_c33(u_ob, [cdr, u_found], Delq_c33_Ret),
		cl_rplacd(Found_Get25, Delq_c33_Ret, TrueResult27),
		LetResult14=TrueResult27
	    ;   get_var(LEnv15, u_ob, Ob_Get26),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "cx$retract-unhash: Strange, I can't seem to unhash ~A"),
			   Ob_Get26
			 ],
			 ElseResult),
		LetResult14=ElseResult
	    )
	;   LetResult14=[]
	).
:- set_opv(f_u_cx_c36_retract_unhash, classof, claz_function),
   set_opv(u_cx_c36_retract_unhash, compile_as, kw_function),
   set_opv(u_cx_c36_retract_unhash, function, f_u_cx_c36_retract_unhash),
   _Ignored4=u_cx_c36_retract_unhash.
/*
:- side_effect(assert_lsp(u_cx_c36_retract_unhash,
			  wl:lambda_def(defun, u_cx_c36_retract_unhash, f_u_cx_c36_retract_unhash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [u_ty_c63, type], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [progn, [if, [not, [u_memq_c63, u_ob, [cdr, u_found]]], [error, '$ARRAY'([*], claz_base_character, "cx$retract-unhash: I can't unhash ~A"), u_ob]], [setf, [cdr, u_found], [u_delq_c33, u_ob, [cdr, u_found]]]], [error, '$ARRAY'([*], claz_base_character, "cx$retract-unhash: Strange, I can't seem to unhash ~A"), u_ob]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retract_unhash,
			  wl:arglist_info(u_cx_c36_retract_unhash, f_u_cx_c36_retract_unhash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retract_unhash,
			  wl:init_args(exact_only, f_u_cx_c36_retract_unhash))).
*/
/*
(defun cx$retrieve-hash (self ob)
  (let ((type (ob$ty ob)))
    (if (and (ty? type)
             (not (ty$instance? ob 'USPECIAL))
             (not (ty$instance? ob 'UVAR)))
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (cdr found)
              nil))
        (ob$get self 'all-obs))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7934 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retrieve-hash',[self,ob],[let,[[type,['ob$ty',ob]]],[if,[and,['ty?',type],[not,['ty$instance?',ob,[quote,'USPECIAL']]],[not,['ty$instance?',ob,[quote,'UVAR']]]],[let,[[found,[assq,type,['ob$get',self,[quote,'type-hashing']]]]],[if,found,[cdr,found],[]]],['ob$get',self,[quote,'all-obs']]]]])
wl:lambda_def(defun, u_cx_c36_retrieve_hash, f_u_cx_c36_retrieve_hash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [and, [u_ty_c63, type], [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uspecial]]], [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uvar]]]], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]).
wl:arglist_info(u_cx_c36_retrieve_hash, f_u_cx_c36_retrieve_hash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retrieve_hash).

/*

### Compiled:  `U::CX$RETRIEVE-HASH` 
*/
f_u_cx_c36_retrieve_hash(Self, Ob, LetResult23) :-
	nop(global_env(Env)),
	Env36=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[bv(type, Type_Init)|Env36],
	f_u_ty_c63(type, IFTEST13),
	(   IFTEST13\==[]
	->  get_var(LEnv, u_ob, Ob_Get),
	    f_u_ty_c36_instance_c63(Ob_Get, u_uspecial, PredArgResult),
	    (   PredArgResult==[]
	    ->  get_var(LEnv, u_ob, Ob_Get19),
		f_u_ty_c36_instance_c63(Ob_Get19, u_uvar, Uvar),
		cl_not(Uvar, TrueResult),
		IFTEST=TrueResult
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    LEnv24=[bv(u_found, Found_Init)|LEnv],
	    get_var(LEnv24, u_found, IFTEST26),
	    (   IFTEST26\==[]
	    ->  get_var(LEnv24, u_found, Found_Get29),
		cl_cdr(Found_Get29, TrueResult30),
		LetResult23=TrueResult30
	    ;   LetResult23=[]
	    )
	;   get_var(LEnv, u_self, Self_Get),
	    f_u_ob_c36_get(Self_Get, u_all_obs, ElseResult),
	    LetResult23=ElseResult
	).
:- set_opv(f_u_cx_c36_retrieve_hash, classof, claz_function),
   set_opv(u_cx_c36_retrieve_hash, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_hash, function, f_u_cx_c36_retrieve_hash),
   _Ignored4=u_cx_c36_retrieve_hash.
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash,
			  wl:lambda_def(defun, u_cx_c36_retrieve_hash, f_u_cx_c36_retrieve_hash, [u_self, u_ob], [[let, [[type, [u_ob_c36_ty, u_ob]]], [if, [and, [u_ty_c63, type], [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uspecial]]], [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uvar]]]], [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash,
			  wl:arglist_info(u_cx_c36_retrieve_hash, f_u_cx_c36_retrieve_hash, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash,
			  wl:init_args(exact_only, f_u_cx_c36_retrieve_hash))).
*/
/*
(defun cx$retrieve-hash-type (self type)
  (let ((found (assq type (ob$get self 'type-hashing))))
     (if found
         (cdr found)
         nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8274 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retrieve-hash-type',[self,type],[let,[[found,[assq,type,['ob$get',self,[quote,'type-hashing']]]]],[if,found,[cdr,found],[]]]])
wl:lambda_def(defun, u_cx_c36_retrieve_hash_type, f_u_cx_c36_retrieve_hash_type, [u_self, type], [[let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]]]).
wl:arglist_info(u_cx_c36_retrieve_hash_type, f_u_cx_c36_retrieve_hash_type, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retrieve_hash_type).

/*

### Compiled:  `U::CX$RETRIEVE-HASH-TYPE` 
*/
f_u_cx_c36_retrieve_hash_type(Self, Type, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_self, Self), bv(type, Type)|Env],
	f_ext_assq(type,
		   [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		   Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env18],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get14),
	    cl_cdr(Found_Get14, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_retrieve_hash_type, classof, claz_function),
   set_opv(u_cx_c36_retrieve_hash_type, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_hash_type, function, f_u_cx_c36_retrieve_hash_type),
   _Ignored4=u_cx_c36_retrieve_hash_type.
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash_type,
			  wl:lambda_def(defun, u_cx_c36_retrieve_hash_type, f_u_cx_c36_retrieve_hash_type, [u_self, type], [[let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash_type,
			  wl:arglist_info(u_cx_c36_retrieve_hash_type, f_u_cx_c36_retrieve_hash_type, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_hash_type,
			  wl:init_args(exact_only, f_u_cx_c36_retrieve_hash_type))).
*/
/*
(defun cx$walk (self proc)
  (yloop (yfor ob in (ob$get self 'all-obs))
        (ydo (funcall proc ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8425 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$walk',[self,proc],[yloop,[yfor,ob,in,['ob$get',self,[quote,'all-obs']]],[ydo,[funcall,proc,ob]]]])
wl:lambda_def(defun, u_cx_c36_walk, f_u_cx_c36_walk, [u_self, u_proc], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [funcall, u_proc, u_ob]]]]).
wl:arglist_info(u_cx_c36_walk, f_u_cx_c36_walk, [u_self, u_proc], arginfo{all:[u_self, u_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_proc], opt:0, req:[u_self, u_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_walk).

/*

### Compiled:  `U::CX$WALK` 
*/
f_u_cx_c36_walk(Self, Proc, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_proc, Proc)|Env],
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    [u_ydo, [funcall, u_proc, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_walk, classof, claz_function),
   set_opv(u_cx_c36_walk, compile_as, kw_function),
   set_opv(u_cx_c36_walk, function, f_u_cx_c36_walk),
   _Ignored4=u_cx_c36_walk.
/*
:- side_effect(assert_lsp(u_cx_c36_walk,
			  wl:lambda_def(defun, u_cx_c36_walk, f_u_cx_c36_walk, [u_self, u_proc], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [funcall, u_proc, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_walk,
			  wl:arglist_info(u_cx_c36_walk, f_u_cx_c36_walk, [u_self, u_proc], arginfo{all:[u_self, u_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_proc], opt:0, req:[u_self, u_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_walk,
			  wl:init_args(exact_only, f_u_cx_c36_walk))).
*/
/*
(defun cx$walk-type (self proc type)
  (if *hashing?*
      (yloop (yfor ob in (cx$retrieve-hash-type self type))
            (ydo (funcall proc ob)))
      (yloop (yfor ob in (ob$get self 'all-obs))
            (ydo (if (eq? type (ob$ty ob))
                    (funcall proc ob))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8532 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$walk-type',[self,proc,type],[if,'*hashing?*',[yloop,[yfor,ob,in,['cx$retrieve-hash-type',self,type]],[ydo,[funcall,proc,ob]]],[yloop,[yfor,ob,in,['ob$get',self,[quote,'all-obs']]],[ydo,[if,['eq?',type,['ob$ty',ob]],[funcall,proc,ob]]]]]])
wl:lambda_def(defun, u_cx_c36_walk_type, f_u_cx_c36_walk_type, [u_self, u_proc, type], [[if, u_xx_hashing_c63_xx, [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_retrieve_hash_type, u_self, type]], [u_ydo, [funcall, u_proc, u_ob]]], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [if, [u_eq_c63, type, [u_ob_c36_ty, u_ob]], [funcall, u_proc, u_ob]]]]]]).
wl:arglist_info(u_cx_c36_walk_type, f_u_cx_c36_walk_type, [u_self, u_proc, type], arginfo{all:[u_self, u_proc, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_proc, type], opt:0, req:[u_self, u_proc, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_walk_type).

/*

### Compiled:  `U::CX$WALK-TYPE` 
*/
f_u_cx_c36_walk_type(Self, Proc, Type, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self), bv(u_proc, Proc), bv(type, Type)|Env],
	get_var(Env14, u_xx_hashing_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ 
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_cx_c36_retrieve_hash_type, u_self, type]
			],
			[u_ydo, [funcall, u_proc, u_ob]]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_all_obs]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			    [funcall, u_proc, u_ob]
			  ]
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_walk_type, classof, claz_function),
   set_opv(u_cx_c36_walk_type, compile_as, kw_function),
   set_opv(u_cx_c36_walk_type, function, f_u_cx_c36_walk_type),
   _Ignored4=u_cx_c36_walk_type.
/*
:- side_effect(assert_lsp(u_cx_c36_walk_type,
			  wl:lambda_def(defun, u_cx_c36_walk_type, f_u_cx_c36_walk_type, [u_self, u_proc, type], [[if, u_xx_hashing_c63_xx, [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_retrieve_hash_type, u_self, type]], [u_ydo, [funcall, u_proc, u_ob]]], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [if, [u_eq_c63, type, [u_ob_c36_ty, u_ob]], [funcall, u_proc, u_ob]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_walk_type,
			  wl:arglist_info(u_cx_c36_walk_type, f_u_cx_c36_walk_type, [u_self, u_proc, type], arginfo{all:[u_self, u_proc, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_proc, type], opt:0, req:[u_self, u_proc, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_walk_type,
			  wl:init_args(exact_only, f_u_cx_c36_walk_type))).
*/
/*
(defun cx$get-all (self)
  (ob$get self 'all-obs))

;
; Use of this will artificially slow down non-hashing, but hashing has
; already been proven without this.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$get-all',[self],['ob$get',self,[quote,'all-obs']]])
wl:lambda_def(defun, u_cx_c36_get_all, f_u_cx_c36_get_all, [u_self], [[u_ob_c36_get, u_self, [quote, u_all_obs]]]).
wl:arglist_info(u_cx_c36_get_all, f_u_cx_c36_get_all, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_get_all).

/*

### Compiled:  `U::CX$GET-ALL` 
*/
f_u_cx_c36_get_all(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_get(Self_Get, u_all_obs, All_obs),
	All_obs=FnResult.
:- set_opv(f_u_cx_c36_get_all, classof, claz_function),
   set_opv(u_cx_c36_get_all, compile_as, kw_function),
   set_opv(u_cx_c36_get_all, function, f_u_cx_c36_get_all),
   _Ignored4=u_cx_c36_get_all.
/*
:- side_effect(assert_lsp(u_cx_c36_get_all,
			  wl:lambda_def(defun, u_cx_c36_get_all, f_u_cx_c36_get_all, [u_self], [[u_ob_c36_get, u_self, [quote, u_all_obs]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_get_all,
			  wl:arglist_info(u_cx_c36_get_all, f_u_cx_c36_get_all, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_get_all,
			  wl:init_args(exact_only, f_u_cx_c36_get_all))).
*/
/*
*/
/*
 Use of this will artificially slow down non-hashing, but hashing has
*/
/*
 already been proven without this.
*/
/*
*/
/*
(defun cx$get-all-ty (self type)
  (if *hashing?*
      (let ((found (assq type (ob$get self 'type-hashing))))
         (if found
             (cdr found)
             nil))
      (yloop (initial (result nil))
             (yfor ob in (ob$get self 'all-obs))
             (ydo (if (eq? type (ob$ty ob))
                      (setq result (cons ob result))))
             (yresult result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8982 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$get-all-ty',[self,type],[if,'*hashing?*',[let,[[found,[assq,type,['ob$get',self,[quote,'type-hashing']]]]],[if,found,[cdr,found],[]]],[yloop,[initial,[result,[]]],[yfor,ob,in,['ob$get',self,[quote,'all-obs']]],[ydo,[if,['eq?',type,['ob$ty',ob]],[setq,result,[cons,ob,result]]]],[yresult,result]]]])
wl:lambda_def(defun, u_cx_c36_get_all_ty, f_u_cx_c36_get_all_ty, [u_self, type], [[if, u_xx_hashing_c63_xx, [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [if, [u_eq_c63, type, [u_ob_c36_ty, u_ob]], [setq, u_result, [cons, u_ob, u_result]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_cx_c36_get_all_ty, f_u_cx_c36_get_all_ty, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_get_all_ty).

/*

### Compiled:  `U::CX$GET-ALL-TY` 
*/
f_u_cx_c36_get_all_ty(Self, Type, LetResult) :-
	nop(global_env(Env)),
	Env23=[bv(u_self, Self), bv(type, Type)|Env],
	get_var(Env23, u_xx_hashing_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    LEnv=[bv(u_found, Found_Init)|Env23],
	    get_var(LEnv, u_found, IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(LEnv, u_found, Found_Get17),
		cl_cdr(Found_Get17, TrueResult),
		LetResult=TrueResult
	    ;   LetResult=[]
	    )
	;   f_u_yloop(
		      [ [u_initial, [u_result, []]],
			
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_all_obs]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			    [setq, u_result, [cons, u_ob, u_result]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    LetResult=ElseResult
	).
:- set_opv(f_u_cx_c36_get_all_ty, classof, claz_function),
   set_opv(u_cx_c36_get_all_ty, compile_as, kw_function),
   set_opv(u_cx_c36_get_all_ty, function, f_u_cx_c36_get_all_ty),
   _Ignored4=u_cx_c36_get_all_ty.
/*
:- side_effect(assert_lsp(u_cx_c36_get_all_ty,
			  wl:lambda_def(defun, u_cx_c36_get_all_ty, f_u_cx_c36_get_all_ty, [u_self, type], [[if, u_xx_hashing_c63_xx, [let, [[u_found, [ext_assq, type, [u_ob_c36_get, u_self, [quote, u_type_hashing]]]]], [if, u_found, [cdr, u_found], []]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [if, [u_eq_c63, type, [u_ob_c36_ty, u_ob]], [setq, u_result, [cons, u_ob, u_result]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_get_all_ty,
			  wl:arglist_info(u_cx_c36_get_all_ty, f_u_cx_c36_get_all_ty, [u_self, type], arginfo{all:[u_self, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, type], opt:0, req:[u_self, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_get_all_ty,
			  wl:init_args(exact_only, f_u_cx_c36_get_all_ty))).
*/
/*
(defun cx$stats (self)
  (format *gate-output* "Hash stats for "(defun cx$stats (self)\n  (format *gate-output* \"Hash stats for ~A~%\" self)\n  (yloop (initial (total (length (ob$get self 'all-obs)))\n                  (count 0)\n                  (len nil))\n         (yfor elem in (ob$get self 'type-hashing))\n         (ydo (setq len (length (cdr elem)))\n              (setq count (+ len count))\n              (format *gate-output* \"~A has ~A entries (~A percent)~%\"\n                      (car elem) len (flonum->fixnum \n                                      (fl* (fl/ (fixnum->flonum len)\n                                               (fixnum->flonum total)) 100.0))))\n         (yresult\n          (format *gate-output*\n                  \"There are ~A non typed-hashed entries (~A percent)~%\"\n                  (- total count)\n                  (flonum->fixnum (fl* (fl/ (fixnum->flonum (- total count))\n                            (fixnum->flonum total)) 100.0)))\n          (format *gate-output* \"Total of ~A entries~%\" total))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:9374 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$stats',[self],[format,'*gate-output*','$STRING'("Hash stats for ~A~%"),self],[yloop,[initial,[total,[length,['ob$get',self,[quote,'all-obs']]]],[count,0],[len,[]]],[yfor,elem,in,['ob$get',self,[quote,'type-hashing']]],[ydo,[setq,len,[length,[cdr,elem]]],[setq,count,[+,len,count]],[format,'*gate-output*','$STRING'("~A has ~A entries (~A percent)~%"),[car,elem],len,['flonum->fixnum',['fl*',['fl/',['fixnum->flonum',len],['fixnum->flonum',total]],100.0]]]],[yresult,[format,'*gate-output*','$STRING'("There are ~A non typed-hashed entries (~A percent)~%"),[-,total,count],['flonum->fixnum',['fl*',['fl/',['fixnum->flonum',[-,total,count]],['fixnum->flonum',total]],100.0]]],[format,'*gate-output*','$STRING'("Total of ~A entries~%"),total]]]])
wl:lambda_def(defun, u_cx_c36_stats, f_u_cx_c36_stats, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Hash stats for ~A~%"), u_self], [u_yloop, [u_initial, [u_total, [length, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [count, 0], [u_len, []]], [u_yfor, u_elem, u_in, [u_ob_c36_get, u_self, [quote, u_type_hashing]]], [u_ydo, [setq, u_len, [length, [cdr, u_elem]]], [setq, count, [+, u_len, count]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~A has ~A entries (~A percent)~%"), [car, u_elem], u_len, [u_flonum_c62_fixnum, [u_fl_xx, [u_fl_c47, [u_fixnum_c62_flonum, u_len], [u_fixnum_c62_flonum, u_total]], 100.0]]]], [u_yresult, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "There are ~A non typed-hashed entries (~A percent)~%"), [-, u_total, count], [u_flonum_c62_fixnum, [u_fl_xx, [u_fl_c47, [u_fixnum_c62_flonum, [-, u_total, count]], [u_fixnum_c62_flonum, u_total]], 100.0]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Total of ~A entries~%"), u_total]]]]).
wl:arglist_info(u_cx_c36_stats, f_u_cx_c36_stats, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_stats).

/*

### Compiled:  `U::CX$STATS` 
*/
f_u_cx_c36_stats(Self, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_self, Self)|Env],
	get_var(Env11, u_self, Self_Get),
	get_var(Env11, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*], claz_base_character, "Hash stats for ~A~%"),
		    Self_Get
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_total,
			[length, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
		      ],
		      [count, 0],
		      [u_len, []]
		    ],
		    
		    [ u_yfor,
		      u_elem,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_type_hashing]]
		    ],
		    
		    [ u_ydo,
		      [setq, u_len, [length, [cdr, u_elem]]],
		      [setq, count, [+, u_len, count]],
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 "~A has ~A entries (~A percent)~%"),
			[car, u_elem],
			u_len,
			
			[ u_flonum_c62_fixnum,
			  
			  [ u_fl_xx,
			    
			    [ u_fl_c47,
			      [u_fixnum_c62_flonum, u_len],
			      [u_fixnum_c62_flonum, u_total]
			    ],
			    100.0
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 "There are ~A non typed-hashed entries (~A percent)~%"),
			[-, u_total, count],
			
			[ u_flonum_c62_fixnum,
			  
			  [ u_fl_xx,
			    
			    [ u_fl_c47,
			      [u_fixnum_c62_flonum, [-, u_total, count]],
			      [u_fixnum_c62_flonum, u_total]
			    ],
			    100.0
			  ]
			]
		      ],
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 "Total of ~A entries~%"),
			u_total
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_stats, classof, claz_function),
   set_opv(u_cx_c36_stats, compile_as, kw_function),
   set_opv(u_cx_c36_stats, function, f_u_cx_c36_stats),
   _Ignored4=u_cx_c36_stats.
/*
:- side_effect(assert_lsp(u_cx_c36_stats,
			  wl:lambda_def(defun, u_cx_c36_stats, f_u_cx_c36_stats, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Hash stats for ~A~%"), u_self], [u_yloop, [u_initial, [u_total, [length, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [count, 0], [u_len, []]], [u_yfor, u_elem, u_in, [u_ob_c36_get, u_self, [quote, u_type_hashing]]], [u_ydo, [setq, u_len, [length, [cdr, u_elem]]], [setq, count, [+, u_len, count]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~A has ~A entries (~A percent)~%"), [car, u_elem], u_len, [u_flonum_c62_fixnum, [u_fl_xx, [u_fl_c47, [u_fixnum_c62_flonum, u_len], [u_fixnum_c62_flonum, u_total]], 100.0]]]], [u_yresult, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "There are ~A non typed-hashed entries (~A percent)~%"), [-, u_total, count], [u_flonum_c62_fixnum, [u_fl_xx, [u_fl_c47, [u_fixnum_c62_flonum, [-, u_total, count]], [u_fixnum_c62_flonum, u_total]], 100.0]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Total of ~A entries~%"), u_total]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_stats,
			  wl:arglist_info(u_cx_c36_stats, f_u_cx_c36_stats, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_stats,
			  wl:init_args(exact_only, f_u_cx_c36_stats))).
*/
/*
(defun cx$assert (self ob)
  (if (and *disallow-non-leaf?*
           (ob$get self 'children))
      (error "Cannot assert "(defun cx$assert (self ob)\n  (if (and *disallow-non-leaf?*\n           (ob$get self 'children))\n      (error \"Cannot assert ~A in non-leaf context ~A.\" ob self))\n  (assert-dbg ob self)\n  (if (cx$true? self ob)\n      (ndbg *gate-dbg* context \"Assert: ~A already true in ~A~%\" ob self)\n      (progn\n       (if *gen-stream*\n            (generate ob\n                      self\n                      (append *global-switches*\n                              (ob$get self 'gen-switches))))\n       (if *hashing?* (cx$assert-hash self ob))\n       (ob$set self 'add-obs (cons ob (ob$get self 'add-obs)))\n       (ob$set self 'all-obs (cons ob (ob$get self 'all-obs)))\n       (if (touchable-fact? ob)\n           (ob$set self 'touched-facts (cons ob (ob$get self 'touched-facts))))\n       (ob$add ob 'top-context self)))\n  ob)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:10339 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$assert',[self,ob],[if,[and,'*disallow-non-leaf?*',['ob$get',self,[quote,children]]],[error,'$STRING'("Cannot assert ~A in non-leaf context ~A."),ob,self]],['assert-dbg',ob,self],[if,['cx$true?',self,ob],[ndbg,'*gate-dbg*',context,'$STRING'("Assert: ~A already true in ~A~%"),ob,self],[progn,[if,'*gen-stream*',[generate,ob,self,[append,'*global-switches*',['ob$get',self,[quote,'gen-switches']]]]],[if,'*hashing?*',['cx$assert-hash',self,ob]],['ob$set',self,[quote,'add-obs'],[cons,ob,['ob$get',self,[quote,'add-obs']]]],['ob$set',self,[quote,'all-obs'],[cons,ob,['ob$get',self,[quote,'all-obs']]]],[if,['touchable-fact?',ob],['ob$set',self,[quote,'touched-facts'],[cons,ob,['ob$get',self,[quote,'touched-facts']]]]],['ob$add',ob,[quote,'top-context'],self]]],ob])
wl:lambda_def(defun, u_cx_c36_assert, f_u_cx_c36_assert, [u_self, u_ob], [[if, [and, u_xx_disallow_non_leaf_c63_xx, [u_ob_c36_get, u_self, [quote, u_children]]], [error, '$ARRAY'([*], claz_base_character, "Cannot assert ~A in non-leaf context ~A."), u_ob, u_self]], [u_assert_dbg, u_ob, u_self], [if, [u_cx_c36_true_c63, u_self, u_ob], [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "Assert: ~A already true in ~A~%"), u_ob, u_self], [progn, [if, u_xx_gen_stream_xx, [u_generate, u_ob, u_self, [append, u_xx_global_switches_xx, [u_ob_c36_get, u_self, [quote, u_gen_switches]]]]], [if, u_xx_hashing_c63_xx, [u_cx_c36_assert_hash, u_self, u_ob]], [u_ob_c36_set, u_self, [quote, u_add_obs], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [if, [u_touchable_fact_c63, u_ob], [u_ob_c36_set, u_self, [quote, u_touched_facts], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]], [u_ob_c36_add, u_ob, [quote, u_top_context], u_self]]], u_ob]).
wl:arglist_info(u_cx_c36_assert, f_u_cx_c36_assert, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_assert).

/*

### Compiled:  `U::CX$ASSERT` 
*/
f_u_cx_c36_assert(Self, Ob, FnResult) :-
	nop(global_env(Env)),
	Env56=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	get_var(Env56, u_xx_disallow_non_leaf_c63_xx, IFTEST9),
	(   IFTEST9\==[]
	->  get_var(Env56, u_self, Self_Get),
	    f_u_ob_c36_get(Self_Get, u_children, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env56, u_ob, Ob_Get),
	    get_var(Env56, u_self, Self_Get15),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"Cannot assert ~A in non-leaf context ~A."),
		       Ob_Get,
		       Self_Get15
		     ],
		     TrueResult16),
	    _248195506=TrueResult16
	;   _248195506=[]
	),
	get_var(Env56, u_ob, Ob_Get17),
	get_var(Env56, u_self, Self_Get18),
	f_u_assert_dbg(Ob_Get17, Self_Get18, Assert_dbg_Ret),
	get_var(Env56, u_ob, Ob_Get22),
	get_var(Env56, u_self, Self_Get21),
	f_u_cx_c36_true_c63(Self_Get21, Ob_Get22, IFTEST19),
	(   IFTEST19\==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				"Assert: ~A already true in ~A~%"),
		       u_ob,
		       u_self
		     ],
		     TrueResult51),
	    _248215348=TrueResult51
	;   get_var(Env56, u_xx_gen_stream_xx, IFTEST23),
	    (   IFTEST23\==[]
	    ->  get_var(Env56, u_ob, Ob_Get26),
		get_var(Env56, u_self, Self_Get27),
		get_var(Env56,
			u_xx_global_switches_xx,
			Xx_global_switches_xx_Get),
		f_u_ob_c36_get(Self_Get27, u_gen_switches, Gen_switches),
		cl_append(Xx_global_switches_xx_Get, Gen_switches, Append_Ret),
		f_u_generate(Ob_Get26, Self_Get27, Append_Ret, TrueResult30),
		_248223768=TrueResult30
	    ;   _248223768=[]
	    ),
	    get_var(Env56, u_xx_hashing_c63_xx, IFTEST31),
	    (   IFTEST31\==[]
	    ->  get_var(Env56, u_ob, Ob_Get35),
		get_var(Env56, u_self, Self_Get34),
		f_u_cx_c36_assert_hash(Self_Get34, Ob_Get35, TrueResult36),
		_248242446=TrueResult36
	    ;   _248242446=[]
	    ),
	    get_var(Env56, u_ob, Ob_Get38),
	    get_var(Env56, u_self, Self_Get37),
	    f_u_ob_c36_get(Self_Get37, u_add_obs, Add_obs),
	    Add_obs61=[Ob_Get38|Add_obs],
	    f_u_ob_c36_set(Self_Get37, u_add_obs, Add_obs61, C36_set_Ret),
	    get_var(Env56, u_ob, Ob_Get41),
	    get_var(Env56, u_self, Self_Get40),
	    f_u_ob_c36_get(Self_Get40, u_all_obs, All_obs),
	    All_obs63=[Ob_Get41|All_obs],
	    f_u_ob_c36_set(Self_Get40, u_all_obs, All_obs63, C36_set_Ret69),
	    f_u_touchable_fact_c63(u_ob, IFTEST43),
	    (   IFTEST43\==[]
	    ->  get_var(Env56, u_ob, Ob_Get46),
		get_var(Env56, u_self, Self_Get45),
		f_u_ob_c36_get(Self_Get45, u_touched_facts, Touched_facts),
		Touched_facts65=[Ob_Get46|Touched_facts],
		f_u_ob_c36_set(Self_Get45,
			       u_touched_facts,
			       Touched_facts65,
			       TrueResult48),
		_248263142=TrueResult48
	    ;   _248263142=[]
	    ),
	    get_var(Env56, u_ob, Ob_Get49),
	    get_var(Env56, u_self, Self_Get50),
	    f_u_ob_c36_add(Ob_Get49, u_top_context, Self_Get50, ElseResult),
	    _248215348=ElseResult
	),
	get_var(Env56, u_ob, Ob_Get53),
	Ob_Get53=FnResult.
:- set_opv(f_u_cx_c36_assert, classof, claz_function),
   set_opv(u_cx_c36_assert, compile_as, kw_function),
   set_opv(u_cx_c36_assert, function, f_u_cx_c36_assert),
   _Ignored4=u_cx_c36_assert.
/*
:- side_effect(assert_lsp(u_cx_c36_assert,
			  wl:lambda_def(defun, u_cx_c36_assert, f_u_cx_c36_assert, [u_self, u_ob], [[if, [and, u_xx_disallow_non_leaf_c63_xx, [u_ob_c36_get, u_self, [quote, u_children]]], [error, '$ARRAY'([*], claz_base_character, "Cannot assert ~A in non-leaf context ~A."), u_ob, u_self]], [u_assert_dbg, u_ob, u_self], [if, [u_cx_c36_true_c63, u_self, u_ob], [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "Assert: ~A already true in ~A~%"), u_ob, u_self], [progn, [if, u_xx_gen_stream_xx, [u_generate, u_ob, u_self, [append, u_xx_global_switches_xx, [u_ob_c36_get, u_self, [quote, u_gen_switches]]]]], [if, u_xx_hashing_c63_xx, [u_cx_c36_assert_hash, u_self, u_ob]], [u_ob_c36_set, u_self, [quote, u_add_obs], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [if, [u_touchable_fact_c63, u_ob], [u_ob_c36_set, u_self, [quote, u_touched_facts], [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_touched_facts]]]]], [u_ob_c36_add, u_ob, [quote, u_top_context], u_self]]], u_ob]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_assert,
			  wl:arglist_info(u_cx_c36_assert, f_u_cx_c36_assert, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_assert,
			  wl:init_args(exact_only, f_u_cx_c36_assert))).
*/
/*
(defun assert-dbg (ob self)
  (ndbg-roman-nl *gate-dbg* rule "Assert "(defun assert-dbg (ob self)\n  (ndbg-roman-nl *gate-dbg* rule \"Assert ~A in ~A\"\n   ob (ob->string self))\n;  (ob$pr ob *gate-dbg* *ob-print-options*)\n;  (do-newline *gate-dbg*)\n)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11152 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'assert-dbg',[ob,self],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Assert ~A in ~A"),ob,['ob->string',self]]])
wl:lambda_def(defun, u_assert_dbg, f_u_assert_dbg, [u_ob, u_self], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Assert ~A in ~A"), u_ob, [u_ob_c62_string, u_self]]]).
wl:arglist_info(u_assert_dbg, f_u_assert_dbg, [u_ob, u_self], arginfo{all:[u_ob, u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_self], opt:0, req:[u_ob, u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_assert_dbg).

/*

### Compiled:  `U::ASSERT-DBG` 
*/
f_u_assert_dbg(Ob, Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_ob, Ob), bv(u_self, Self)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Assert ~A in ~A"),
			    u_ob,
			    [u_ob_c62_string, u_self]
			  ],
			  Roman_nl_Ret),
	Roman_nl_Ret=FnResult.
:- set_opv(f_u_assert_dbg, classof, claz_function),
   set_opv(u_assert_dbg, compile_as, kw_function),
   set_opv(u_assert_dbg, function, f_u_assert_dbg),
   _Ignored4=u_assert_dbg.
/*
:- side_effect(assert_lsp(u_assert_dbg,
			  wl:lambda_def(defun, u_assert_dbg, f_u_assert_dbg, [u_ob, u_self], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Assert ~A in ~A"), u_ob, [u_ob_c62_string, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_assert_dbg,
			  wl:arglist_info(u_assert_dbg, f_u_assert_dbg, [u_ob, u_self], arginfo{all:[u_ob, u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_self], opt:0, req:[u_ob, u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_assert_dbg,
			  wl:init_args(exact_only, f_u_assert_dbg))).
*/
/*
  (ob$pr ob *gate-dbg* *ob-print-options*)
*/
/*
  (do-newline *gate-dbg*)
*/
/*
(defun retract-dbg (ob self)
  (ndbg-roman-nl *gate-dbg* rule "Retract "(defun retract-dbg (ob self)\n  (ndbg-roman-nl *gate-dbg* rule \"Retract ~A in ~A\"\n   ob (ob->string self))\n;  (ob$pr ob *gate-dbg* *ob-print-options*)\n;  (do-newline *gate-dbg*)\n)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11330 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'retract-dbg',[ob,self],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Retract ~A in ~A"),ob,['ob->string',self]]])
wl:lambda_def(defun, u_retract_dbg, f_u_retract_dbg, [u_ob, u_self], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Retract ~A in ~A"), u_ob, [u_ob_c62_string, u_self]]]).
wl:arglist_info(u_retract_dbg, f_u_retract_dbg, [u_ob, u_self], arginfo{all:[u_ob, u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_self], opt:0, req:[u_ob, u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_retract_dbg).

/*

### Compiled:  `U::RETRACT-DBG` 
*/
f_u_retract_dbg(Ob, Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_ob, Ob), bv(u_self, Self)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Retract ~A in ~A"),
			    u_ob,
			    [u_ob_c62_string, u_self]
			  ],
			  Roman_nl_Ret),
	Roman_nl_Ret=FnResult.
:- set_opv(f_u_retract_dbg, classof, claz_function),
   set_opv(u_retract_dbg, compile_as, kw_function),
   set_opv(u_retract_dbg, function, f_u_retract_dbg),
   _Ignored4=u_retract_dbg.
/*
:- side_effect(assert_lsp(u_retract_dbg,
			  wl:lambda_def(defun, u_retract_dbg, f_u_retract_dbg, [u_ob, u_self], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Retract ~A in ~A"), u_ob, [u_ob_c62_string, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_retract_dbg,
			  wl:arglist_info(u_retract_dbg, f_u_retract_dbg, [u_ob, u_self], arginfo{all:[u_ob, u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_self], opt:0, req:[u_ob, u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_retract_dbg,
			  wl:init_args(exact_only, f_u_retract_dbg))).
*/
/*
  (ob$pr ob *gate-dbg* *ob-print-options*)
*/
/*
  (do-newline *gate-dbg*)
*/
/*
(setq *ctxt-unify-semantics?* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11510 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ctxt-unify-semantics?*',[]])
:- set_var(AEnv, setq, u_xx_ctxt_unify_semantics_c63_xx, []).
/*
(defun cx$retract (self ob)
  (if (and *disallow-non-leaf?*
           (ob$get self 'children))
      (error "Cannot retract "(defun cx$retract (self ob)\n  (if (and *disallow-non-leaf?*\n           (ob$get self 'children))\n      (error \"Cannot retract ~A in non-leaf context ~A.\" ob self))\n  (if (not (cx$true? self ob))\n      (ndbg *gate-dbg* context \"Retract: ~A already false in ~A~%\"\n            ob self)\n      (let ((found nil))\n        (retract-dbg ob self)\n        (if (memq? self (ob$gets ob 'top-context))\n            (ob$remove ob 'top-context self))\n        (if *ctxt-unify-semantics?*\n            (setq found (or (memq ob (ob$get self 'add-obs))\n                           (mem-empty-unify ob (ob$get self 'add-obs) self)))\n            (setq found (memq ob (ob$get self 'add-obs))))\n        (if found\n            (progn\n             (if *hashing?* (cx$retract-unhash self (car found)))\n             (ob$set self 'touched-facts (delq! (car found)\n                                                (ob$get self 'touched-facts)))\n             (ob$set self 'add-obs (delq! (car found) (ob$get self 'add-obs)))\n             (ob$set self 'all-obs (delq! (car found)\n                                           (ob$get self 'all-obs))))\n            (progn\n             (if *ctxt-unify-semantics?*\n                 (setq found (or (memq ob (ob$get self 'all-obs))\n                                (mem-empty-unify ob (ob$get self 'all-obs)\n                                                 self)))\n                 (setq found (memq ob (ob$get self 'all-obs))))\n             (if found\n                 (progn\n                  (if *hashing?* (cx$retract-unhash self (car found)))\n                  (ob$set self 'remove-obs (cons (car found)\n                                                  (ob$get self 'remove-obs)))\n                  ; If touched-facts inheritence is disabled, the line\n                  ; below is unnecessary, because a touched-fact would\n                  ; have to be in add-obs.\n                  (ob$set self 'touched-facts (delq! (car found)\n                                                     (ob$get self\n                                                             'touched-facts)))\n                  (if *ctxt-unify-semantics?*\n                      (ob$set self 'all-obs (del-unify! (car found)\n                                                         (ob$get self 'all-obs)\n                                                         self))\n                      (ob$set self 'all-obs (delq! (car found)\n                                                    (ob$get self 'all-obs))))\n                  ; Todo: For unify semantics, I still don't see why we\n                  ; have to unify here. Won't we get an inconsistent state\n                  ; if more than one one element matches? Besides, we've\n                  ; already done the unify.\n;                  (yloop (yfor justificand in\n;                             (cx$justificands self (car found)))\n;                         (ydo (ndbg *gate-dbg* context\n;                            \"Retracting dependent assertion ~A~%\" justificand)\n;                             (cx$retract self justificand)))\n                  )\n                 (error \"Retract: cannot find ~A.\" ob))))))\n  ob)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retract',[self,ob],[if,[and,'*disallow-non-leaf?*',['ob$get',self,[quote,children]]],[error,'$STRING'("Cannot retract ~A in non-leaf context ~A."),ob,self]],[if,[not,['cx$true?',self,ob]],[ndbg,'*gate-dbg*',context,'$STRING'("Retract: ~A already false in ~A~%"),ob,self],[let,[[found,[]]],['retract-dbg',ob,self],[if,['memq?',self,['ob$gets',ob,[quote,'top-context']]],['ob$remove',ob,[quote,'top-context'],self]],[if,'*ctxt-unify-semantics?*',[setq,found,[or,[memq,ob,['ob$get',self,[quote,'add-obs']]],['mem-empty-unify',ob,['ob$get',self,[quote,'add-obs']],self]]],[setq,found,[memq,ob,['ob$get',self,[quote,'add-obs']]]]],[if,found,[progn,[if,'*hashing?*',['cx$retract-unhash',self,[car,found]]],['ob$set',self,[quote,'touched-facts'],['delq!',[car,found],['ob$get',self,[quote,'touched-facts']]]],['ob$set',self,[quote,'add-obs'],['delq!',[car,found],['ob$get',self,[quote,'add-obs']]]],['ob$set',self,[quote,'all-obs'],['delq!',[car,found],['ob$get',self,[quote,'all-obs']]]]],[progn,[if,'*ctxt-unify-semantics?*',[setq,found,[or,[memq,ob,['ob$get',self,[quote,'all-obs']]],['mem-empty-unify',ob,['ob$get',self,[quote,'all-obs']],self]]],[setq,found,[memq,ob,['ob$get',self,[quote,'all-obs']]]]],[if,found,[progn,[if,'*hashing?*',['cx$retract-unhash',self,[car,found]]],['ob$set',self,[quote,'remove-obs'],[cons,[car,found],['ob$get',self,[quote,'remove-obs']]]],['ob$set',self,[quote,'touched-facts'],['delq!',[car,found],['ob$get',self,[quote,'touched-facts']]]],[if,'*ctxt-unify-semantics?*',['ob$set',self,[quote,'all-obs'],['del-unify!',[car,found],['ob$get',self,[quote,'all-obs']],self]],['ob$set',self,[quote,'all-obs'],['delq!',[car,found],['ob$get',self,[quote,'all-obs']]]]]],[error,'$STRING'("Retract: cannot find ~A."),ob]]]]]],ob])
wl:lambda_def(defun, u_cx_c36_retract, f_u_cx_c36_retract, [u_self, u_ob], [[if, [and, u_xx_disallow_non_leaf_c63_xx, [u_ob_c36_get, u_self, [quote, u_children]]], [error, '$ARRAY'([*], claz_base_character, "Cannot retract ~A in non-leaf context ~A."), u_ob, u_self]], [if, [not, [u_cx_c36_true_c63, u_self, u_ob]], [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "Retract: ~A already false in ~A~%"), u_ob, u_self], [let, [[u_found, []]], [u_retract_dbg, u_ob, u_self], [if, [u_memq_c63, u_self, [u_ob_c36_gets, u_ob, [quote, u_top_context]]], [u_ob_c36_remove, u_ob, [quote, u_top_context], u_self]], [if, u_xx_ctxt_unify_semantics_c63_xx, [setq, u_found, [or, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]], [u_mem_empty_unify, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]], u_self]]], [setq, u_found, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]]]], [if, u_found, [progn, [if, u_xx_hashing_c63_xx, [u_cx_c36_retract_unhash, u_self, [car, u_found]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_touched_facts]]]], [u_ob_c36_set, u_self, [quote, u_add_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_add_obs]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]], [progn, [if, u_xx_ctxt_unify_semantics_c63_xx, [setq, u_found, [or, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_mem_empty_unify, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]], u_self]]], [setq, u_found, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]]]], [if, u_found, [progn, [if, u_xx_hashing_c63_xx, [u_cx_c36_retract_unhash, u_self, [car, u_found]]], [u_ob_c36_set, u_self, [quote, u_remove_obs], [cons, [car, u_found], [u_ob_c36_get, u_self, [quote, u_remove_obs]]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_touched_facts]]]], [if, u_xx_ctxt_unify_semantics_c63_xx, [u_ob_c36_set, u_self, [quote, u_all_obs], [u_del_unify_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]], u_self]], [u_ob_c36_set, u_self, [quote, u_all_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]], [error, '$ARRAY'([*], claz_base_character, "Retract: cannot find ~A."), u_ob]]]]]], u_ob]).
wl:arglist_info(u_cx_c36_retract, f_u_cx_c36_retract, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retract).

/*

### Compiled:  `U::CX$RETRACT` 
*/
f_u_cx_c36_retract(Self, Ob, FnResult) :-
	nop(global_env(Env)),
	Env87=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	get_var(Env87, u_xx_disallow_non_leaf_c63_xx, IFTEST9),
	(   IFTEST9\==[]
	->  get_var(Env87, u_self, Self_Get),
	    f_u_ob_c36_get(Self_Get, u_children, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env87, u_ob, Ob_Get),
	    get_var(Env87, u_self, Self_Get15),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"Cannot retract ~A in non-leaf context ~A."),
		       Ob_Get,
		       Self_Get15
		     ],
		     TrueResult16),
	    _253010550=TrueResult16
	;   _253010550=[]
	),
	get_var(Env87, u_ob, Ob_Get19),
	get_var(Env87, u_self, Self_Get18),
	f_u_cx_c36_true_c63(Self_Get18, Ob_Get19, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				"Retract: ~A already false in ~A~%"),
		       u_ob,
		       u_self
		     ],
		     TrueResult82),
	    TrueResult78=TrueResult82
	;   LEnv=[bv(u_found, [])|Env87],
	    get_var(LEnv, u_ob, Ob_Get25),
	    get_var(LEnv, u_self, Self_Get26),
	    f_u_retract_dbg(Ob_Get25, Self_Get26, Retract_dbg_Ret),
	    f_u_memq_c63(u_self,
			 [u_ob_c36_gets, u_ob, [quote, u_top_context]],
			 IFTEST27),
	    (   IFTEST27\==[]
	    ->  get_var(LEnv, u_ob, Ob_Get29),
		get_var(LEnv, u_self, Self_Get30),
		f_u_ob_c36_remove(Ob_Get29,
				  u_top_context,
				  Self_Get30,
				  TrueResult31),
		_253058718=TrueResult31
	    ;   _253058718=[]
	    ),
	    get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST32),
	    (   IFTEST32\==[]
	    ->  (   f_ext_memq(u_ob,
			       [u_ob_c36_get, u_self, [quote, u_add_obs]],
			       FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult37=FORM1_Res
		->  true
		;   f_u_mem_empty_unify(u_ob,
					
					[ u_ob_c36_get,
					  u_self,
					  [quote, u_add_obs]
					],
					u_self,
					Self90),
		    TrueResult37=Self90
		),
		set_var(LEnv, u_found, TrueResult37),
		_253069502=TrueResult37
	    ;   f_ext_memq(u_ob,
			   [u_ob_c36_get, u_self, [quote, u_add_obs]],
			   ElseResult),
		set_var(LEnv, u_found, ElseResult),
		_253069502=ElseResult
	    ),
	    get_var(LEnv, u_found, IFTEST39),
	    (   IFTEST39\==[]
	    ->  get_var(LEnv, u_xx_hashing_c63_xx, IFTEST42),
		(   IFTEST42\==[]
		->  get_var(LEnv, u_found, Found_Get46),
		    get_var(LEnv, u_self, Self_Get45),
		    cl_car(Found_Get46, Car_Ret),
		    f_u_cx_c36_retract_unhash(Self_Get45, Car_Ret, TrueResult47),
		    _253095526=TrueResult47
		;   _253095526=[]
		),
		get_var(LEnv, u_self, Self_Get48),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_touched_facts]],
			     Touched_facts),
		f_u_ob_c36_set(Self_Get48,
			       u_touched_facts,
			       Touched_facts,
			       C36_set_Ret),
		get_var(LEnv, u_self, Self_Get49),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_add_obs]],
			     Add_obs),
		f_u_ob_c36_set(Self_Get49, u_add_obs, Add_obs, C36_set_Ret103),
		get_var(LEnv, u_self, Self_Get50),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_all_obs]],
			     All_obs),
		f_u_ob_c36_set(Self_Get50, u_all_obs, All_obs, TrueResult80),
		TrueResult78=TrueResult80
	    ;   get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST51),
		(   IFTEST51\==[]
		->  (   f_ext_memq(u_ob,
				   [u_ob_c36_get, u_self, [quote, u_all_obs]],
				   FORM1_Res54),
			FORM1_Res54\==[],
			TrueResult55=FORM1_Res54
		    ->  true
		    ;   f_u_mem_empty_unify(u_ob,
					    
					    [ u_ob_c36_get,
					      u_self,
					      [quote, u_all_obs]
					    ],
					    u_self,
					    Self94),
			TrueResult55=Self94
		    ),
		    set_var(LEnv, u_found, TrueResult55),
		    _253187830=TrueResult55
		;   f_ext_memq(u_ob,
			       [u_ob_c36_get, u_self, [quote, u_all_obs]],
			       ElseResult56),
		    set_var(LEnv, u_found, ElseResult56),
		    _253187830=ElseResult56
		),
		get_var(LEnv, u_found, IFTEST57),
		(   IFTEST57\==[]
		->  get_var(LEnv, u_xx_hashing_c63_xx, IFTEST60),
		    (   IFTEST60\==[]
		    ->  get_var(LEnv, u_found, Found_Get64),
			get_var(LEnv, u_self, Self_Get63),
			cl_car(Found_Get64, Car_Ret104),
			f_u_cx_c36_retract_unhash(Self_Get63,
						  Car_Ret104,
						  TrueResult65),
			_253205120=TrueResult65
		    ;   _253205120=[]
		    ),
		    get_var(LEnv, u_found, Found_Get67),
		    get_var(LEnv, u_self, Self_Get66),
		    cl_car(Found_Get67, Car_Ret105),
		    get_var(LEnv, u_self, Self_Get68),
		    f_u_ob_c36_get(Self_Get68, u_remove_obs, Remove_obs),
		    Remove_obs96=[Car_Ret105|Remove_obs],
		    f_u_ob_c36_set(Self_Get66,
				   u_remove_obs,
				   Remove_obs96,
				   C36_set_Ret106),
		    get_var(LEnv, u_self, Self_Get69),
		    f_u_delq_c33([car, u_found],
				 [u_ob_c36_get, u_self, [quote, u_touched_facts]],
				 Touched_facts97),
		    f_u_ob_c36_set(Self_Get69,
				   u_touched_facts,
				   Touched_facts97,
				   C36_set_Ret107),
		    get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST70),
		    (   IFTEST70\==[]
		    ->  get_var(LEnv, u_self, Self_Get73),
			f_u_del_unify_c33([car, u_found],
					  
					  [ u_ob_c36_get,
					    u_self,
					    [quote, u_all_obs]
					  ],
					  u_self,
					  Self98),
			f_u_ob_c36_set(Self_Get73,
				       u_all_obs,
				       Self98,
				       TrueResult75),
			TrueResult78=TrueResult75
		    ;   get_var(LEnv, u_self, Self_Get74),
			f_u_delq_c33([car, u_found],
				     [u_ob_c36_get, u_self, [quote, u_all_obs]],
				     All_obs99),
			f_u_ob_c36_set(Self_Get74,
				       u_all_obs,
				       All_obs99,
				       ElseResult76),
			TrueResult78=ElseResult76
		    )
		;   get_var(LEnv, u_ob, Ob_Get77),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					"Retract: cannot find ~A."),
			       Ob_Get77
			     ],
			     ElseResult79),
		    TrueResult78=ElseResult79
		)
	    )
	),
	get_var(Env87, u_ob, Ob_Get84),
	Ob_Get84=FnResult.
:- set_opv(f_u_cx_c36_retract, classof, claz_function),
   set_opv(u_cx_c36_retract, compile_as, kw_function),
   set_opv(u_cx_c36_retract, function, f_u_cx_c36_retract),
   _Ignored4=u_cx_c36_retract.
/*
:- side_effect(assert_lsp(u_cx_c36_retract,
			  wl:lambda_def(defun, u_cx_c36_retract, f_u_cx_c36_retract, [u_self, u_ob], [[if, [and, u_xx_disallow_non_leaf_c63_xx, [u_ob_c36_get, u_self, [quote, u_children]]], [error, '$ARRAY'([*], claz_base_character, "Cannot retract ~A in non-leaf context ~A."), u_ob, u_self]], [if, [not, [u_cx_c36_true_c63, u_self, u_ob]], [u_ndbg, u_xx_gate_dbg_xx, u_context, '$ARRAY'([*], claz_base_character, "Retract: ~A already false in ~A~%"), u_ob, u_self], [let, [[u_found, []]], [u_retract_dbg, u_ob, u_self], [if, [u_memq_c63, u_self, [u_ob_c36_gets, u_ob, [quote, u_top_context]]], [u_ob_c36_remove, u_ob, [quote, u_top_context], u_self]], [if, u_xx_ctxt_unify_semantics_c63_xx, [setq, u_found, [or, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]], [u_mem_empty_unify, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]], u_self]]], [setq, u_found, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]]]], [if, u_found, [progn, [if, u_xx_hashing_c63_xx, [u_cx_c36_retract_unhash, u_self, [car, u_found]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_touched_facts]]]], [u_ob_c36_set, u_self, [quote, u_add_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_add_obs]]]], [u_ob_c36_set, u_self, [quote, u_all_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]], [progn, [if, u_xx_ctxt_unify_semantics_c63_xx, [setq, u_found, [or, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_mem_empty_unify, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]], u_self]]], [setq, u_found, [ext_memq, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]]]], [if, u_found, [progn, [if, u_xx_hashing_c63_xx, [u_cx_c36_retract_unhash, u_self, [car, u_found]]], [u_ob_c36_set, u_self, [quote, u_remove_obs], [cons, [car, u_found], [u_ob_c36_get, u_self, [quote, u_remove_obs]]]], [u_ob_c36_set, u_self, [quote, u_touched_facts], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_touched_facts]]]], [if, u_xx_ctxt_unify_semantics_c63_xx, [u_ob_c36_set, u_self, [quote, u_all_obs], [u_del_unify_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]], u_self]], [u_ob_c36_set, u_self, [quote, u_all_obs], [u_delq_c33, [car, u_found], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]], [error, '$ARRAY'([*], claz_base_character, "Retract: cannot find ~A."), u_ob]]]]]], u_ob]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retract,
			  wl:arglist_info(u_cx_c36_retract, f_u_cx_c36_retract, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retract,
			  wl:init_args(exact_only, f_u_cx_c36_retract))).
*/
/*
 If touched-facts inheritence is disabled, the line
*/
/*
 below is unnecessary, because a touched-fact would
*/
/*
 have to be in add-obs.
*/
/*
 Todo: For unify semantics, I still don't see why we
*/
/*
 have to unify here. Won't we get an inconsistent state
*/
/*
 if more than one one element matches? Besides, we've
*/
/*
 already done the unify.
*/
/*
                  (yloop (yfor justificand in
*/
/*
                             (cx$justificands self (car found)))
*/
/*
                         (ydo (ndbg *gate-dbg* context
*/
/*
                            "Retracting dependent assertion "                            \"Retracting dependent assertion ~A~%\" justificand)".
*/
/*
                             (cx$retract self justificand)))
*/
/*
(defun cx$copy (self parent)
  (yloop (initial (new-context (cx$sprout parent))
                  (new-ob nil))
         (yfor ob in (ob$get self 'all-obs))
         (ydo (setq new-ob ob) ; was (ob$copy ob) but this would kill all links!
;             (ob$set new-ob 'top-context new-context)
;             (ob$removes new-ob 'top-context)
             (cx$assert new-context new-ob))
         (yresult new-context)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:14693 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$copy',[self,parent],[yloop,[initial,['new-context',['cx$sprout',parent]],['new-ob',[]]],[yfor,ob,in,['ob$get',self,[quote,'all-obs']]],[ydo,[setq,'new-ob',ob],['cx$assert','new-context','new-ob']],[yresult,'new-context']]])
wl:lambda_def(defun, u_cx_c36_copy, f_u_cx_c36_copy, [u_self, u_parent], [[u_yloop, [u_initial, [u_new_context, [u_cx_c36_sprout, u_parent]], [u_new_ob, []]], [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [setq, u_new_ob, u_ob], [u_cx_c36_assert, u_new_context, u_new_ob]], [u_yresult, u_new_context]]]).
wl:arglist_info(u_cx_c36_copy, f_u_cx_c36_copy, [u_self, u_parent], arginfo{all:[u_self, u_parent], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_parent], opt:0, req:[u_self, u_parent], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_copy).

/*

### Compiled:  `U::CX$COPY` 
*/
f_u_cx_c36_copy(Self, Parent, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_parent, Parent)|Env],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_new_context, [u_cx_c36_sprout, u_parent]],
		      [u_new_ob, []]
		    ],
		    [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    
		    [ u_ydo,
		      [setq, u_new_ob, u_ob],
		      [u_cx_c36_assert, u_new_context, u_new_ob]
		    ],
		    [u_yresult, u_new_context]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_copy, classof, claz_function),
   set_opv(u_cx_c36_copy, compile_as, kw_function),
   set_opv(u_cx_c36_copy, function, f_u_cx_c36_copy),
   _Ignored4=u_cx_c36_copy.
/*
:- side_effect(assert_lsp(u_cx_c36_copy,
			  wl:lambda_def(defun, u_cx_c36_copy, f_u_cx_c36_copy, [u_self, u_parent], [[u_yloop, [u_initial, [u_new_context, [u_cx_c36_sprout, u_parent]], [u_new_ob, []]], [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]], [u_ydo, [setq, u_new_ob, u_ob], [u_cx_c36_assert, u_new_context, u_new_ob]], [u_yresult, u_new_context]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_copy,
			  wl:arglist_info(u_cx_c36_copy, f_u_cx_c36_copy, [u_self, u_parent], arginfo{all:[u_self, u_parent], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_parent], opt:0, req:[u_self, u_parent], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_copy,
			  wl:init_args(exact_only, f_u_cx_c36_copy))).
*/
/*
 was (ob$copy ob) but this would kill all links!
*/
/*
             (ob$set new-ob 'top-context new-context)
*/
/*
             (ob$removes new-ob 'top-context)
*/
/*
(setq *retrieve-ignore-slots* nil)

; Returns ( <bd: (<retrieved-ob> (<var> <value>)...)> ...)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15112 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*retrieve-ignore-slots*',[]])
:- set_var(AEnv, setq, u_xx_retrieve_ignore_slots_xx, []).
/*
 Returns ( <bd: (<retrieved-ob> (<var> <value>)...)> ...)
*/
/*
(defun cx$retrieve (self pattern)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?*
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ydo (setq bindings (ob$unify-cx1 pattern ob *empty-bd*
                              *retrieve-ignore-slots* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15207 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retrieve',[self,pattern],[yloop,[initial,[result,[]],[bindings,[]]],[yfor,ob,in,[if,'*hashing?*',['cx$retrieve-hash',self,pattern],['ob$get',self,[quote,'all-obs']]]],[ydo,[setq,bindings,['ob$unify-cx1',pattern,ob,'*empty-bd*','*retrieve-ignore-slots*',self]],[if,bindings,[setq,result,[cons,[cons,ob,[cdr,bindings]],result]]]],[yresult,result]]])
wl:lambda_def(defun, u_cx_c36_retrieve, f_u_cx_c36_retrieve, [u_self, u_pattern], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx1, u_pattern, u_ob, u_xx_empty_bd_xx, u_xx_retrieve_ignore_slots_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_cx_c36_retrieve, f_u_cx_c36_retrieve, [u_self, u_pattern], arginfo{all:[u_self, u_pattern], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern], opt:0, req:[u_self, u_pattern], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retrieve).

/*

### Compiled:  `U::CX$RETRIEVE` 
*/
f_u_cx_c36_retrieve(Self, Pattern, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_pattern, Pattern)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63_xx,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx1,
			  u_pattern,
			  u_ob,
			  u_xx_empty_bd_xx,
			  u_xx_retrieve_ignore_slots_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve, classof, claz_function),
   set_opv(u_cx_c36_retrieve, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve, function, f_u_cx_c36_retrieve),
   _Ignored4=u_cx_c36_retrieve.
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve,
			  wl:lambda_def(defun, u_cx_c36_retrieve, f_u_cx_c36_retrieve, [u_self, u_pattern], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx1, u_pattern, u_ob, u_xx_empty_bd_xx, u_xx_retrieve_ignore_slots_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve,
			  wl:arglist_info(u_cx_c36_retrieve, f_u_cx_c36_retrieve, [u_self, u_pattern], arginfo{all:[u_self, u_pattern], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern], opt:0, req:[u_self, u_pattern], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve,
			  wl:init_args(exact_only, f_u_cx_c36_retrieve))).
*/
/*
(defun cx$retrieve-bd (self pattern bd)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?*
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ydo (setq bindings (ob$unify-cx1 pattern ob bd
                              *retrieve-ignore-slots* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15704 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retrieve-bd',[self,pattern,bd],[yloop,[initial,[result,[]],[bindings,[]]],[yfor,ob,in,[if,'*hashing?*',['cx$retrieve-hash',self,pattern],['ob$get',self,[quote,'all-obs']]]],[ydo,[setq,bindings,['ob$unify-cx1',pattern,ob,bd,'*retrieve-ignore-slots*',self]],[if,bindings,[setq,result,[cons,[cons,ob,[cdr,bindings]],result]]]],[yresult,result]]])
wl:lambda_def(defun, u_cx_c36_retrieve_bd, f_u_cx_c36_retrieve_bd, [u_self, u_pattern, u_bd], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx1, u_pattern, u_ob, u_bd, u_xx_retrieve_ignore_slots_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_cx_c36_retrieve_bd, f_u_cx_c36_retrieve_bd, [u_self, u_pattern, u_bd], arginfo{all:[u_self, u_pattern, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern, u_bd], opt:0, req:[u_self, u_pattern, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retrieve_bd).

/*

### Compiled:  `U::CX$RETRIEVE-BD` 
*/
f_u_cx_c36_retrieve_bd(Self, Pattern, Bd, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_pattern, Pattern), bv(u_bd, Bd)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63_xx,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx1,
			  u_pattern,
			  u_ob,
			  u_bd,
			  u_xx_retrieve_ignore_slots_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_bd, classof, claz_function),
   set_opv(u_cx_c36_retrieve_bd, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_bd, function, f_u_cx_c36_retrieve_bd),
   _Ignored4=u_cx_c36_retrieve_bd.
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_bd,
			  wl:lambda_def(defun, u_cx_c36_retrieve_bd, f_u_cx_c36_retrieve_bd, [u_self, u_pattern, u_bd], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx1, u_pattern, u_ob, u_bd, u_xx_retrieve_ignore_slots_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_bd,
			  wl:arglist_info(u_cx_c36_retrieve_bd, f_u_cx_c36_retrieve_bd, [u_self, u_pattern, u_bd], arginfo{all:[u_self, u_pattern, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern, u_bd], opt:0, req:[u_self, u_pattern, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_bd,
			  wl:init_args(exact_only, f_u_cx_c36_retrieve_bd))).
*/
/*
(defun cx$retrieve-number (self pattern number)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ywhile (< (length result) number))
         (ydo (setq bindings (ob$unify-cx pattern ob *empty-bd* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16199 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$retrieve-number',[self,pattern,number],[yloop,[initial,[result,[]],[bindings,[]]],[yfor,ob,in,[if,'*hashing?',['cx$retrieve-hash',self,pattern],['ob$get',self,[quote,'all-obs']]]],[ywhile,[<,[length,result],number]],[ydo,[setq,bindings,['ob$unify-cx',pattern,ob,'*empty-bd*',self]],[if,bindings,[setq,result,[cons,[cons,ob,[cdr,bindings]],result]]]],[yresult,result]]])
wl:lambda_def(defun, u_cx_c36_retrieve_number, f_u_cx_c36_retrieve_number, [u_self, u_pattern, number], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ywhile, [<, [length, u_result], number]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx, u_pattern, u_ob, u_xx_empty_bd_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_cx_c36_retrieve_number, f_u_cx_c36_retrieve_number, [u_self, u_pattern, number], arginfo{all:[u_self, u_pattern, number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern, number], opt:0, req:[u_self, u_pattern, number], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_retrieve_number).

/*

### Compiled:  `U::CX$RETRIEVE-NUMBER` 
*/
f_u_cx_c36_retrieve_number(Self, Pattern, Number, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_pattern, Pattern), bv(number, Number)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    [u_ywhile, [<, [length, u_result], number]],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx,
			  u_pattern,
			  u_ob,
			  u_xx_empty_bd_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_number, classof, claz_function),
   set_opv(u_cx_c36_retrieve_number, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_number, function, f_u_cx_c36_retrieve_number),
   _Ignored4=u_cx_c36_retrieve_number.
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_number,
			  wl:lambda_def(defun, u_cx_c36_retrieve_number, f_u_cx_c36_retrieve_number, [u_self, u_pattern, number], [[u_yloop, [u_initial, [u_result, []], [bindings, []]], [u_yfor, u_ob, u_in, [if, u_xx_hashing_c63, [u_cx_c36_retrieve_hash, u_self, u_pattern], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [u_ywhile, [<, [length, u_result], number]], [u_ydo, [setq, bindings, [u_ob_c36_unify_cx, u_pattern, u_ob, u_xx_empty_bd_xx, u_self]], [if, bindings, [setq, u_result, [cons, [cons, u_ob, [cdr, bindings]], u_result]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_number,
			  wl:arglist_info(u_cx_c36_retrieve_number, f_u_cx_c36_retrieve_number, [u_self, u_pattern, number], arginfo{all:[u_self, u_pattern, number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_pattern, number], opt:0, req:[u_self, u_pattern, number], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_retrieve_number,
			  wl:init_args(exact_only, f_u_cx_c36_retrieve_number))).
*/
/*
(defun cx$leaf? (self)
  (null? (ob$get self 'children)))

;(defun cx$justificands (self ob)
;  (yloop (initial (result nil))
;         (yfor justificand in (ob$gets ob 'justifies))
;         (ydo (if (cx$true? self justificand)
;                  (setq result (cons justificand result))))
;         (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$leaf?',[self],['null?',['ob$get',self,[quote,children]]]])
wl:lambda_def(defun, u_cx_c36_leaf_c63, f_u_cx_c36_leaf_c63, [u_self], [[u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]]]).
wl:arglist_info(u_cx_c36_leaf_c63, f_u_cx_c36_leaf_c63, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_leaf_c63).

/*

### Compiled:  `U::CX$LEAF?` 
*/
f_u_cx_c36_leaf_c63(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], Null_c63_Ret),
	Null_c63_Ret=FnResult.
:- set_opv(f_u_cx_c36_leaf_c63, classof, claz_function),
   set_opv(u_cx_c36_leaf_c63, compile_as, kw_function),
   set_opv(u_cx_c36_leaf_c63, function, f_u_cx_c36_leaf_c63),
   _Ignored4=u_cx_c36_leaf_c63.
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_c63,
			  wl:lambda_def(defun, u_cx_c36_leaf_c63, f_u_cx_c36_leaf_c63, [u_self], [[u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_c63,
			  wl:arglist_info(u_cx_c36_leaf_c63, f_u_cx_c36_leaf_c63, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_leaf_c63,
			  wl:init_args(exact_only, f_u_cx_c36_leaf_c63))).
*/
/*
(defun cx$justificands (self ob)
*/
/*
  (yloop (initial (result nil))
*/
/*
         (yfor justificand in (ob$gets ob 'justifies))
*/
/*
         (ydo (if (cx$true? self justificand)
*/
/*
                  (setq result (cons justificand result))))
*/
/*
         (yresult result)))
*/
/*
(defun cx$true? (self ob)
 (if *ctxt-unify-semantics?*
  (let ((top-contexts nil)
        (obs nil))
    (if (and (not (ob$get self 'pseudo-sprout?))
             (setq top-contexts (ob$gets ob 'top-context)))
        (yloop
         (initial (result nil) (found? nil))
         (yfor top-context in top-contexts)
         (yuntil (or result found?))
         (ydo
          (cond
           ((eq? top-context self) (setq result t))
           ((memq? top-context (ob$get self 'ancestors))
            (setq found? t) (setq result t)
            (yloop
             (yfor context in (cdr
                              (memq top-context
                                    (reverse (cons self
                                                   (ob$get self 'ancestors))))))
                  (ydo (cond ((memq? ob (ob$get context 'add-obs))
                             (setq result t))
                            ((memq? ob (ob$get context 'remove-obs))
                             (setq result nil))))))))
         (yresult result))
        (progn
         (setq obs (if *hashing?* (cx$retrieve-hash self ob)
                       (ob$get self 'all-obs)))
         (if *ctxt-unify-semantics?*
             (or (memq? ob obs) (mem-empty-unify? ob obs self))
             (memq? ob obs)))))
  (memq? ob (if *hashing?* (cx$retrieve-hash self ob)
                            (ob$get self 'all-obs)))))

; (if (memq? ob &remove-obs) nil <else>) for above?

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:17019 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$true?',[self,ob],[if,'*ctxt-unify-semantics?*',[let,[['top-contexts',[]],[obs,[]]],[if,[and,[not,['ob$get',self,[quote,'pseudo-sprout?']]],[setq,'top-contexts',['ob$gets',ob,[quote,'top-context']]]],[yloop,[initial,[result,[]],['found?',[]]],[yfor,'top-context',in,'top-contexts'],[yuntil,[or,result,'found?']],[ydo,[cond,[['eq?','top-context',self],[setq,result,t]],[['memq?','top-context',['ob$get',self,[quote,ancestors]]],[setq,'found?',t],[setq,result,t],[yloop,[yfor,context,in,[cdr,[memq,'top-context',[reverse,[cons,self,['ob$get',self,[quote,ancestors]]]]]]],[ydo,[cond,[['memq?',ob,['ob$get',context,[quote,'add-obs']]],[setq,result,t]],[['memq?',ob,['ob$get',context,[quote,'remove-obs']]],[setq,result,[]]]]]]]]],[yresult,result]],[progn,[setq,obs,[if,'*hashing?*',['cx$retrieve-hash',self,ob],['ob$get',self,[quote,'all-obs']]]],[if,'*ctxt-unify-semantics?*',[or,['memq?',ob,obs],['mem-empty-unify?',ob,obs,self]],['memq?',ob,obs]]]]],['memq?',ob,[if,'*hashing?*',['cx$retrieve-hash',self,ob],['ob$get',self,[quote,'all-obs']]]]]])
wl:lambda_def(defun, u_cx_c36_true_c63, f_u_cx_c36_true_c63, [u_self, u_ob], [[if, u_xx_ctxt_unify_semantics_c63_xx, [let, [[u_top_contexts, []], [u_obs, []]], [if, [and, [not, [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]]], [setq, u_top_contexts, [u_ob_c36_gets, u_ob, [quote, u_top_context]]]], [u_yloop, [u_initial, [u_result, []], [u_found_c63, []]], [u_yfor, u_top_context, u_in, u_top_contexts], [u_yuntil, [or, u_result, u_found_c63]], [u_ydo, [cond, [[u_eq_c63, u_top_context, u_self], [setq, u_result, t]], [[u_memq_c63, u_top_context, [u_ob_c36_get, u_self, [quote, u_ancestors]]], [setq, u_found_c63, t], [setq, u_result, t], [u_yloop, [u_yfor, u_context, u_in, [cdr, [ext_memq, u_top_context, [reverse, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]]]]], [u_ydo, [cond, [[u_memq_c63, u_ob, [u_ob_c36_get, u_context, [quote, u_add_obs]]], [setq, u_result, t]], [[u_memq_c63, u_ob, [u_ob_c36_get, u_context, [quote, u_remove_obs]]], [setq, u_result, []]]]]]]]], [u_yresult, u_result]], [progn, [setq, u_obs, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_ob], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [if, u_xx_ctxt_unify_semantics_c63_xx, [or, [u_memq_c63, u_ob, u_obs], [u_mem_empty_unify_c63, u_ob, u_obs, u_self]], [u_memq_c63, u_ob, u_obs]]]]], [u_memq_c63, u_ob, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_ob], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]]).
wl:arglist_info(u_cx_c36_true_c63, f_u_cx_c36_true_c63, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_true_c63).

/*

### Compiled:  `U::CX$TRUE?` 
*/
f_u_cx_c36_true_c63(Self, Ob, TrueResult34) :-
	nop(global_env(Env)),
	Env42=[bv(u_self, Self), bv(u_ob, Ob)|Env],
	get_var(Env42, u_xx_ctxt_unify_semantics_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  LEnv=[bv(u_top_contexts, []), bv(u_obs, [])|Env42],
	    get_var(LEnv, u_self, Self_Get),
	    f_u_ob_c36_get(Self_Get, u_pseudo_sprout_c63, PredArgResult),
	    (   PredArgResult==[]
	    ->  get_var(LEnv, u_ob, Ob_Get),
		f_u_ob_c36_gets(Ob_Get, u_top_context, TrueResult),
		set_var(LEnv, u_top_contexts, TrueResult),
		IFTEST13=TrueResult
	    ;   IFTEST13=[]
	    ),
	    (   IFTEST13\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_result, []], [u_found_c63, []]],
			    [u_yfor, u_top_context, u_in, u_top_contexts],
			    [u_yuntil, [or, u_result, u_found_c63]],
			    
			    [ u_ydo,
			      
			      [ cond,
				
				[ [u_eq_c63, u_top_context, u_self],
				  [setq, u_result, t]
				],
				
				[ 
				  [ u_memq_c63,
				    u_top_context,
				    [u_ob_c36_get, u_self, [quote, u_ancestors]]
				  ],
				  [setq, u_found_c63, t],
				  [setq, u_result, t],
				  
				  [ u_yloop,
				    
				    [ u_yfor,
				      u_context,
				      u_in,
				      
				      [ cdr,
					
					[ ext_memq,
					  u_top_context,
					  
					  [ reverse,
					    
					    [ cons,
					      u_self,
					      
					      [ u_ob_c36_get,
						u_self,
						[quote, u_ancestors]
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_ydo,
				      
				      [ cond,
					
					[ 
					  [ u_memq_c63,
					    u_ob,
					    
					    [ u_ob_c36_get,
					      u_context,
					      [quote, u_add_obs]
					    ]
					  ],
					  [setq, u_result, t]
					],
					
					[ 
					  [ u_memq_c63,
					    u_ob,
					    
					    [ u_ob_c36_get,
					      u_context,
					      [quote, u_remove_obs]
					    ]
					  ],
					  [setq, u_result, []]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_result]
			  ],
			  TrueResult36),
		TrueResult34=TrueResult36
	    ;   get_var(LEnv, u_xx_hashing_c63_xx, IFTEST22),
		(   IFTEST22\==[]
		->  get_var(LEnv, u_ob, Ob_Get26),
		    get_var(LEnv, u_self, Self_Get25),
		    f_u_cx_c36_retrieve_hash(Self_Get25, Ob_Get26, TrueResult28),
		    Obs=TrueResult28
		;   get_var(LEnv, u_self, Self_Get27),
		    f_u_ob_c36_get(Self_Get27, u_all_obs, ElseResult),
		    Obs=ElseResult
		),
		set_var(LEnv, u_obs, Obs),
		get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST30),
		(   IFTEST30\==[]
		->  (   f_u_memq_c63(u_ob, u_obs, FORM1_Res),
			FORM1_Res\==[],
			TrueResult34=FORM1_Res
		    ->  true
		    ;   f_u_mem_empty_unify_c63(u_ob, u_obs, u_self, Self46),
			TrueResult34=Self46
		    )
		;   f_u_memq_c63(u_ob, u_obs, ElseResult35),
		    TrueResult34=ElseResult35
		)
	    )
	;   f_u_memq_c63(u_ob,
			 
			 [ if,
			   u_xx_hashing_c63_xx,
			   [u_cx_c36_retrieve_hash, u_self, u_ob],
			   [u_ob_c36_get, u_self, [quote, u_all_obs]]
			 ],
			 ElseResult39),
	    TrueResult34=ElseResult39
	).
:- set_opv(f_u_cx_c36_true_c63, classof, claz_function),
   set_opv(u_cx_c36_true_c63, compile_as, kw_function),
   set_opv(u_cx_c36_true_c63, function, f_u_cx_c36_true_c63),
   _Ignored4=u_cx_c36_true_c63.
/*
:- side_effect(assert_lsp(u_cx_c36_true_c63,
			  wl:lambda_def(defun, u_cx_c36_true_c63, f_u_cx_c36_true_c63, [u_self, u_ob], [[if, u_xx_ctxt_unify_semantics_c63_xx, [let, [[u_top_contexts, []], [u_obs, []]], [if, [and, [not, [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]]], [setq, u_top_contexts, [u_ob_c36_gets, u_ob, [quote, u_top_context]]]], [u_yloop, [u_initial, [u_result, []], [u_found_c63, []]], [u_yfor, u_top_context, u_in, u_top_contexts], [u_yuntil, [or, u_result, u_found_c63]], [u_ydo, [cond, [[u_eq_c63, u_top_context, u_self], [setq, u_result, t]], [[u_memq_c63, u_top_context, [u_ob_c36_get, u_self, [quote, u_ancestors]]], [setq, u_found_c63, t], [setq, u_result, t], [u_yloop, [u_yfor, u_context, u_in, [cdr, [ext_memq, u_top_context, [reverse, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]]]]], [u_ydo, [cond, [[u_memq_c63, u_ob, [u_ob_c36_get, u_context, [quote, u_add_obs]]], [setq, u_result, t]], [[u_memq_c63, u_ob, [u_ob_c36_get, u_context, [quote, u_remove_obs]]], [setq, u_result, []]]]]]]]], [u_yresult, u_result]], [progn, [setq, u_obs, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_ob], [u_ob_c36_get, u_self, [quote, u_all_obs]]]], [if, u_xx_ctxt_unify_semantics_c63_xx, [or, [u_memq_c63, u_ob, u_obs], [u_mem_empty_unify_c63, u_ob, u_obs, u_self]], [u_memq_c63, u_ob, u_obs]]]]], [u_memq_c63, u_ob, [if, u_xx_hashing_c63_xx, [u_cx_c36_retrieve_hash, u_self, u_ob], [u_ob_c36_get, u_self, [quote, u_all_obs]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_true_c63,
			  wl:arglist_info(u_cx_c36_true_c63, f_u_cx_c36_true_c63, [u_self, u_ob], arginfo{all:[u_self, u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_ob], opt:0, req:[u_self, u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_true_c63,
			  wl:init_args(exact_only, f_u_cx_c36_true_c63))).
*/
/*
 (if (memq? ob &remove-obs) nil <else>) for above?
*/
/*
(defun cx$hypothesize (self retracts asserts)
  (let ((new-context (cx$sprout self)))
    (yloop (yfor assertion in retracts)
           (ydo (cx$retract new-context assertion)))
    (yloop (yfor assertion in asserts)
           (ydo (cx$assert new-context assertion)))
    new-context))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18475 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$hypothesize',[self,retracts,asserts],[let,[['new-context',['cx$sprout',self]]],[yloop,[yfor,assertion,in,retracts],[ydo,['cx$retract','new-context',assertion]]],[yloop,[yfor,assertion,in,asserts],[ydo,['cx$assert','new-context',assertion]]],'new-context']])
wl:lambda_def(defun, u_cx_c36_hypothesize, f_u_cx_c36_hypothesize, [u_self, u_retracts, u_asserts], [[let, [[u_new_context, [u_cx_c36_sprout, u_self]]], [u_yloop, [u_yfor, u_assertion, u_in, u_retracts], [u_ydo, [u_cx_c36_retract, u_new_context, u_assertion]]], [u_yloop, [u_yfor, u_assertion, u_in, u_asserts], [u_ydo, [u_cx_c36_assert, u_new_context, u_assertion]]], u_new_context]]).
wl:arglist_info(u_cx_c36_hypothesize, f_u_cx_c36_hypothesize, [u_self, u_retracts, u_asserts], arginfo{all:[u_self, u_retracts, u_asserts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_retracts, u_asserts], opt:0, req:[u_self, u_retracts, u_asserts], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_hypothesize).

/*

### Compiled:  `U::CX$HYPOTHESIZE` 
*/
f_u_cx_c36_hypothesize(Self, Retracts, Asserts, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_self, Self), bv(u_retracts, Retracts), bv(u_asserts, Asserts)|Env],
	get_var(Env15, u_self, Self_Get),
	f_u_cx_c36_sprout(Self_Get, New_context_Init),
	LEnv=[bv(u_new_context, New_context_Init)|Env15],
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_retracts],
		    [u_ydo, [u_cx_c36_retract, u_new_context, u_assertion]]
		  ],
		  Yloop_Ret),
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_asserts],
		    [u_ydo, [u_cx_c36_assert, u_new_context, u_assertion]]
		  ],
		  Yloop_Ret20),
	get_var(LEnv, u_new_context, New_context_Get),
	New_context_Get=FnResult.
:- set_opv(f_u_cx_c36_hypothesize, classof, claz_function),
   set_opv(u_cx_c36_hypothesize, compile_as, kw_function),
   set_opv(u_cx_c36_hypothesize, function, f_u_cx_c36_hypothesize),
   _Ignored4=u_cx_c36_hypothesize.
/*
:- side_effect(assert_lsp(u_cx_c36_hypothesize,
			  wl:lambda_def(defun, u_cx_c36_hypothesize, f_u_cx_c36_hypothesize, [u_self, u_retracts, u_asserts], [[let, [[u_new_context, [u_cx_c36_sprout, u_self]]], [u_yloop, [u_yfor, u_assertion, u_in, u_retracts], [u_ydo, [u_cx_c36_retract, u_new_context, u_assertion]]], [u_yloop, [u_yfor, u_assertion, u_in, u_asserts], [u_ydo, [u_cx_c36_assert, u_new_context, u_assertion]]], u_new_context]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_hypothesize,
			  wl:arglist_info(u_cx_c36_hypothesize, f_u_cx_c36_hypothesize, [u_self, u_retracts, u_asserts], arginfo{all:[u_self, u_retracts, u_asserts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_retracts, u_asserts], opt:0, req:[u_self, u_retracts, u_asserts], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_hypothesize,
			  wl:init_args(exact_only, f_u_cx_c36_hypothesize))).
*/
/*
(defun cx$generate (self)
  (format *gen-stream* ""(defun cx$generate (self)\n  (format *gen-stream* \"~%-----~%Contents (generate) of ~A:~%\" self)\n  (yloop (yfor ob in (cx$sorted-all-obs self))\n         (ydo (generate ob nil self)))\n  (format *gen-stream* \"~&-----~%\"))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18764 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$generate',[self],[format,'*gen-stream*','$STRING'("~%-----~%Contents (generate) of ~A:~%"),self],[yloop,[yfor,ob,in,['cx$sorted-all-obs',self]],[ydo,[generate,ob,[],self]]],[format,'*gen-stream*','$STRING'("~&-----~%")]])
wl:lambda_def(defun, u_cx_c36_generate, f_u_cx_c36_generate, [u_self], [[format, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "~%-----~%Contents (generate) of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [u_generate, u_ob, [], u_self]]], [format, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "~&-----~%")]]).
wl:arglist_info(u_cx_c36_generate, f_u_cx_c36_generate, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_generate).

/*

### Compiled:  `U::CX$GENERATE` 
*/
f_u_cx_c36_generate(Self, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self)|Env],
	get_var(Env12, u_self, Self_Get),
	get_var(Env12, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	cl_format(
		  [ Xx_gen_stream_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     "~%-----~%Contents (generate) of ~A:~%"),
		    Self_Get
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    [u_ydo, [u_generate, u_ob, [], u_self]]
		  ],
		  Yloop_Ret),
	get_var(Env12, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get9),
	cl_format(
		  [ Xx_gen_stream_xx_Get9,
		    '$ARRAY'([*], claz_base_character, "~&-----~%")
		  ],
		  Format_Ret16),
	Format_Ret16=FnResult.
:- set_opv(f_u_cx_c36_generate, classof, claz_function),
   set_opv(u_cx_c36_generate, compile_as, kw_function),
   set_opv(u_cx_c36_generate, function, f_u_cx_c36_generate),
   _Ignored4=u_cx_c36_generate.
/*
:- side_effect(assert_lsp(u_cx_c36_generate,
			  wl:lambda_def(defun, u_cx_c36_generate, f_u_cx_c36_generate, [u_self], [[format, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "~%-----~%Contents (generate) of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [u_generate, u_ob, [], u_self]]], [format, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "~&-----~%")]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_generate,
			  wl:arglist_info(u_cx_c36_generate, f_u_cx_c36_generate, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_generate,
			  wl:init_args(exact_only, f_u_cx_c36_generate))).
*/
/*
(defun cx$print (self)
  (format *gate-output* ""(defun cx$print (self)\n  (format *gate-output* \"~&-----~%Contents of ~A:~%\" self)\n  (yloop (yfor ob in (cx$sorted-all-obs self))\n         (ydo (progn (format t \"#{~A: \" (ob$name ob)) (ob$print ob *gate-output*) (format t \"}\"))\n             (newline *gate-output*)))\n  (format *gate-output* \"-----~%\")\n  nil)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18983 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$print',[self],[format,'*gate-output*','$STRING'("~&-----~%Contents of ~A:~%"),self],[yloop,[yfor,ob,in,['cx$sorted-all-obs',self]],[ydo,[progn,[format,t,'$STRING'("#{~A: "),['ob$name',ob]],['ob$print',ob,'*gate-output*'],[format,t,'$STRING'("}")]],[newline,'*gate-output*']]],[format,'*gate-output*','$STRING'("-----~%")],[]])
wl:lambda_def(defun, u_cx_c36_print, f_u_cx_c36_print, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Contents of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [progn, [format, t, '$ARRAY'([*], claz_base_character, "#{~A: "), [u_ob_c36_name, u_ob]], [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [format, t, '$ARRAY'([*], claz_base_character, "}")]], [u_newline, u_xx_gate_output_xx]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "-----~%")], []]).
wl:arglist_info(u_cx_c36_print, f_u_cx_c36_print, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_print).

/*

### Compiled:  `U::CX$PRINT` 
*/
f_u_cx_c36_print(Self, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self)|Env],
	get_var(Env12, u_self, Self_Get),
	get_var(Env12, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     "~&-----~%Contents of ~A:~%"),
		    Self_Get
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    
		    [ u_ydo,
		      
		      [ progn,
			
			[ format,
			  t,
			  '$ARRAY'([*], claz_base_character, "#{~A: "),
			  [u_ob_c36_name, u_ob]
			],
			[u_ob_c36_print, u_ob, u_xx_gate_output_xx],
			[format, t, '$ARRAY'([*], claz_base_character, "}")]
		      ],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env12, u_xx_gate_output_xx, Xx_gate_output_xx_Get9),
	cl_format(
		  [ Xx_gate_output_xx_Get9,
		    '$ARRAY'([*], claz_base_character, "-----~%")
		  ],
		  Format_Ret16),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print, classof, claz_function),
   set_opv(u_cx_c36_print, compile_as, kw_function),
   set_opv(u_cx_c36_print, function, f_u_cx_c36_print),
   _Ignored4=u_cx_c36_print.
/*
:- side_effect(assert_lsp(u_cx_c36_print,
			  wl:lambda_def(defun, u_cx_c36_print, f_u_cx_c36_print, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Contents of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [progn, [format, t, '$ARRAY'([*], claz_base_character, "#{~A: "), [u_ob_c36_name, u_ob]], [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [format, t, '$ARRAY'([*], claz_base_character, "}")]], [u_newline, u_xx_gate_output_xx]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "-----~%")], []]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print,
			  wl:arglist_info(u_cx_c36_print, f_u_cx_c36_print, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print,
			  wl:init_args(exact_only, f_u_cx_c36_print))).
*/
/*
(defun cx$print-actions (self)
  (format *gate-output* ""(defun cx$print-actions (self)\n  (format *gate-output* \"~&-----~%Contents (actions) of ~A:~%\" self)\n  (yloop (yfor ob in (cx$sorted-all-obs self))\n         (ydo (if (ty$instance? ob 'action)\n                  (progn\n                   (ob$print ob *gate-output*)\n                   (newline *gate-output*)))))\n  (format *gate-output* \"-----~%\")\n  nil)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:19292 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$print-actions',[self],[format,'*gate-output*','$STRING'("~&-----~%Contents (actions) of ~A:~%"),self],[yloop,[yfor,ob,in,['cx$sorted-all-obs',self]],[ydo,[if,['ty$instance?',ob,[quote,action]],[progn,['ob$print',ob,'*gate-output*'],[newline,'*gate-output*']]]]],[format,'*gate-output*','$STRING'("-----~%")],[]])
wl:lambda_def(defun, u_cx_c36_print_actions, f_u_cx_c36_print_actions, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Contents (actions) of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [if, [u_ty_c36_instance_c63, u_ob, [quote, u_action]], [progn, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "-----~%")], []]).
wl:arglist_info(u_cx_c36_print_actions, f_u_cx_c36_print_actions, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_print_actions).

/*

### Compiled:  `U::CX$PRINT-ACTIONS` 
*/
f_u_cx_c36_print_actions(Self, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self)|Env],
	get_var(Env12, u_self, Self_Get),
	get_var(Env12, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     "~&-----~%Contents (actions) of ~A:~%"),
		    Self_Get
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, u_ob, [quote, u_action]],
			
			[ progn,
			  [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
			  [u_newline, u_xx_gate_output_xx]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env12, u_xx_gate_output_xx, Xx_gate_output_xx_Get9),
	cl_format(
		  [ Xx_gate_output_xx_Get9,
		    '$ARRAY'([*], claz_base_character, "-----~%")
		  ],
		  Format_Ret16),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print_actions, classof, claz_function),
   set_opv(u_cx_c36_print_actions, compile_as, kw_function),
   set_opv(u_cx_c36_print_actions, function, f_u_cx_c36_print_actions),
   _Ignored4=u_cx_c36_print_actions.
/*
:- side_effect(assert_lsp(u_cx_c36_print_actions,
			  wl:lambda_def(defun, u_cx_c36_print_actions, f_u_cx_c36_print_actions, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Contents (actions) of ~A:~%"), u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]], [u_ydo, [if, [u_ty_c36_instance_c63, u_ob, [quote, u_action]], [progn, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "-----~%")], []]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_actions,
			  wl:arglist_info(u_cx_c36_print_actions, f_u_cx_c36_print_actions, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_actions,
			  wl:init_args(exact_only, f_u_cx_c36_print_actions))).
*/
/*
(defun cx$print-diffs (self)
  (format *gate-output* ""(defun cx$print-diffs (self)\n  (format *gate-output* \"~&-----~%Differential contents of ~A:~%\" self)\n  (format *gate-output* \"~&Additions:~%\")\n  (yloop (yfor ob in (ob$get self 'add-obs))\n        (ydo (ob$print ob *gate-output*)\n             (newline *gate-output*)))\n  (format *gate-output* \"~&Removals:~%\")\n  (yloop (yfor ob in (ob$get self 'remove-obs))\n        (ydo (ob$print ob *gate-output*)\n             (newline *gate-output*)))\n  nil)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:19645 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$print-diffs',[self],[format,'*gate-output*','$STRING'("~&-----~%Differential contents of ~A:~%"),self],[format,'*gate-output*','$STRING'("~&Additions:~%")],[yloop,[yfor,ob,in,['ob$get',self,[quote,'add-obs']]],[ydo,['ob$print',ob,'*gate-output*'],[newline,'*gate-output*']]],[format,'*gate-output*','$STRING'("~&Removals:~%")],[yloop,[yfor,ob,in,['ob$get',self,[quote,'remove-obs']]],[ydo,['ob$print',ob,'*gate-output*'],[newline,'*gate-output*']]],[]])
wl:lambda_def(defun, u_cx_c36_print_diffs, f_u_cx_c36_print_diffs, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Differential contents of ~A:~%"), u_self], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&Additions:~%")], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_add_obs]]], [u_ydo, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&Removals:~%")], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_remove_obs]]], [u_ydo, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]], []]).
wl:arglist_info(u_cx_c36_print_diffs, f_u_cx_c36_print_diffs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_print_diffs).

/*

### Compiled:  `U::CX$PRINT-DIFFS` 
*/
f_u_cx_c36_print_diffs(Self, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_self, Self)|Env],
	get_var(Env13, u_self, Self_Get),
	get_var(Env13, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     "~&-----~%Differential contents of ~A:~%"),
		    Self_Get
		  ],
		  Format_Ret),
	get_var(Env13, u_xx_gate_output_xx, Xx_gate_output_xx_Get9),
	cl_format(
		  [ Xx_gate_output_xx_Get9,
		    '$ARRAY'([*], claz_base_character, "~&Additions:~%")
		  ],
		  Format_Ret16),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_add_obs]]],
		    
		    [ u_ydo,
		      [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env13, u_xx_gate_output_xx, Xx_gate_output_xx_Get10),
	cl_format(
		  [ Xx_gate_output_xx_Get10,
		    '$ARRAY'([*], claz_base_character, "~&Removals:~%")
		  ],
		  Format_Ret18),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_remove_obs]]
		    ],
		    
		    [ u_ydo,
		      [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret19),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print_diffs, classof, claz_function),
   set_opv(u_cx_c36_print_diffs, compile_as, kw_function),
   set_opv(u_cx_c36_print_diffs, function, f_u_cx_c36_print_diffs),
   _Ignored4=u_cx_c36_print_diffs.
/*
:- side_effect(assert_lsp(u_cx_c36_print_diffs,
			  wl:lambda_def(defun, u_cx_c36_print_diffs, f_u_cx_c36_print_diffs, [u_self], [[format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&-----~%Differential contents of ~A:~%"), u_self], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&Additions:~%")], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_add_obs]]], [u_ydo, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&Removals:~%")], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_remove_obs]]], [u_ydo, [u_ob_c36_print, u_ob, u_xx_gate_output_xx], [u_newline, u_xx_gate_output_xx]]], []]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_diffs,
			  wl:arglist_info(u_cx_c36_print_diffs, f_u_cx_c36_print_diffs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_diffs,
			  wl:init_args(exact_only, f_u_cx_c36_print_diffs))).
*/
/*
(defun cx$print-ancestors (self)
  (yloop (yfor context in (reverse (cons self (ob$get self 'ancestors))))
         (ydo (cx$print-diffs context))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20090 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$print-ancestors',[self],[yloop,[yfor,context,in,[reverse,[cons,self,['ob$get',self,[quote,ancestors]]]]],[ydo,['cx$print-diffs',context]]]])
wl:lambda_def(defun, u_cx_c36_print_ancestors, f_u_cx_c36_print_ancestors, [u_self], [[u_yloop, [u_yfor, u_context, u_in, [reverse, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]]], [u_ydo, [u_cx_c36_print_diffs, u_context]]]]).
wl:arglist_info(u_cx_c36_print_ancestors, f_u_cx_c36_print_ancestors, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_print_ancestors).

/*

### Compiled:  `U::CX$PRINT-ANCESTORS` 
*/
f_u_cx_c36_print_ancestors(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_context,
		      u_in,
		      
		      [ reverse,
			
			[ cons,
			  u_self,
			  [u_ob_c36_get, u_self, [quote, u_ancestors]]
			]
		      ]
		    ],
		    [u_ydo, [u_cx_c36_print_diffs, u_context]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_print_ancestors, classof, claz_function),
   set_opv(u_cx_c36_print_ancestors, compile_as, kw_function),
   set_opv(u_cx_c36_print_ancestors, function, f_u_cx_c36_print_ancestors),
   _Ignored4=u_cx_c36_print_ancestors.
/*
:- side_effect(assert_lsp(u_cx_c36_print_ancestors,
			  wl:lambda_def(defun, u_cx_c36_print_ancestors, f_u_cx_c36_print_ancestors, [u_self], [[u_yloop, [u_yfor, u_context, u_in, [reverse, [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]]], [u_ydo, [u_cx_c36_print_diffs, u_context]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_ancestors,
			  wl:arglist_info(u_cx_c36_print_ancestors, f_u_cx_c36_print_ancestors, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_print_ancestors,
			  wl:init_args(exact_only, f_u_cx_c36_print_ancestors))).
*/
/*
(defun cx$show-descendants (self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$show-descendants1 ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20240 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$show-descendants',[self],[yloop,[yfor,ob,in,['ob$get',self,[quote,children]]],[ydo,['cx$show-descendants1',ob]]]])
wl:lambda_def(defun, u_cx_c36_show_descendants, f_u_cx_c36_show_descendants, [u_self], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_show_descendants1, u_ob]]]]).
wl:arglist_info(u_cx_c36_show_descendants, f_u_cx_c36_show_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_show_descendants).

/*

### Compiled:  `U::CX$SHOW-DESCENDANTS` 
*/
f_u_cx_c36_show_descendants(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_show_descendants, classof, claz_function),
   set_opv(u_cx_c36_show_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_show_descendants, function, f_u_cx_c36_show_descendants),
   _Ignored4=u_cx_c36_show_descendants.
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants,
			  wl:lambda_def(defun, u_cx_c36_show_descendants, f_u_cx_c36_show_descendants, [u_self], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_show_descendants1, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants,
			  wl:arglist_info(u_cx_c36_show_descendants, f_u_cx_c36_show_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants,
			  wl:init_args(exact_only, f_u_cx_c36_show_descendants))).
*/
/*
(defun cx$show-descendants1 (self)
  (ob$unhide self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$show-descendants1 ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20364 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$show-descendants1',[self],['ob$unhide',self],[yloop,[yfor,ob,in,['ob$get',self,[quote,children]]],[ydo,['cx$show-descendants1',ob]]]])
wl:lambda_def(defun, u_cx_c36_show_descendants1, f_u_cx_c36_show_descendants1, [u_self], [[u_ob_c36_unhide, u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_show_descendants1, u_ob]]]]).
wl:arglist_info(u_cx_c36_show_descendants1, f_u_cx_c36_show_descendants1, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_show_descendants1).

/*

### Compiled:  `U::CX$SHOW-DESCENDANTS1` 
*/
f_u_cx_c36_show_descendants1(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_unhide(Self_Get, C36_unhide_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_show_descendants1, classof, claz_function),
   set_opv(u_cx_c36_show_descendants1, compile_as, kw_function),
   set_opv(u_cx_c36_show_descendants1, function, f_u_cx_c36_show_descendants1),
   _Ignored4=u_cx_c36_show_descendants1.
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants1,
			  wl:lambda_def(defun, u_cx_c36_show_descendants1, f_u_cx_c36_show_descendants1, [u_self], [[u_ob_c36_unhide, u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_show_descendants1, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants1,
			  wl:arglist_info(u_cx_c36_show_descendants1, f_u_cx_c36_show_descendants1, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_show_descendants1,
			  wl:init_args(exact_only, f_u_cx_c36_show_descendants1))).
*/
/*
(defun cx$unshow-descendants (self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$unshow-descendants1 ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20508 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$unshow-descendants',[self],[yloop,[yfor,ob,in,['ob$get',self,[quote,children]]],[ydo,['cx$unshow-descendants1',ob]]]])
wl:lambda_def(defun, u_cx_c36_unshow_descendants, f_u_cx_c36_unshow_descendants, [u_self], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]]]).
wl:arglist_info(u_cx_c36_unshow_descendants, f_u_cx_c36_unshow_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_unshow_descendants).

/*

### Compiled:  `U::CX$UNSHOW-DESCENDANTS` 
*/
f_u_cx_c36_unshow_descendants(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_unshow_descendants, classof, claz_function),
   set_opv(u_cx_c36_unshow_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_unshow_descendants, function, f_u_cx_c36_unshow_descendants),
   _Ignored4=u_cx_c36_unshow_descendants.
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants,
			  wl:lambda_def(defun, u_cx_c36_unshow_descendants, f_u_cx_c36_unshow_descendants, [u_self], [[u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants,
			  wl:arglist_info(u_cx_c36_unshow_descendants, f_u_cx_c36_unshow_descendants, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants,
			  wl:init_args(exact_only, f_u_cx_c36_unshow_descendants))).
*/
/*
(defun cx$unshow-descendants1 (self)
  (ob$hide self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$unshow-descendants1 ob))))

;
; Context sensitive links
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20636 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'cx$unshow-descendants1',[self],['ob$hide',self],[yloop,[yfor,ob,in,['ob$get',self,[quote,children]]],[ydo,['cx$unshow-descendants1',ob]]]])
wl:lambda_def(defun, u_cx_c36_unshow_descendants1, f_u_cx_c36_unshow_descendants1, [u_self], [[u_ob_c36_hide, u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]]]).
wl:arglist_info(u_cx_c36_unshow_descendants1, f_u_cx_c36_unshow_descendants1, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c36_unshow_descendants1).

/*

### Compiled:  `U::CX$UNSHOW-DESCENDANTS1` 
*/
f_u_cx_c36_unshow_descendants1(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_ob_c36_hide(Self_Get, C36_hide_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_unshow_descendants1, classof, claz_function),
   set_opv(u_cx_c36_unshow_descendants1, compile_as, kw_function),
   set_opv(u_cx_c36_unshow_descendants1,
	   function,
	   f_u_cx_c36_unshow_descendants1),
   _Ignored4=u_cx_c36_unshow_descendants1.
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants1,
			  wl:lambda_def(defun, u_cx_c36_unshow_descendants1, f_u_cx_c36_unshow_descendants1, [u_self], [[u_ob_c36_hide, u_self], [u_yloop, [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]], [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants1,
			  wl:arglist_info(u_cx_c36_unshow_descendants1, f_u_cx_c36_unshow_descendants1, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c36_unshow_descendants1,
			  wl:init_args(exact_only, f_u_cx_c36_unshow_descendants1))).
*/
/*
*/
/*
 Context sensitive links
*/
/*
*/
/*
(ob$decl-inverses 'linked-to 'linked-to-of)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20813 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$decl-inverses',[quote,'linked-to'],[quote,'linked-to-of']])
:- f_u_ob_c36_decl_inverses(u_linked_to, u_linked_to_of, _Ignored4).
/*
(ob$decl-inverses 'linked-from 'linked-from-of)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20857 **********************/
:-lisp_compile_to_prolog(pkg_user,['ob$decl-inverses',[quote,'linked-from'],[quote,'linked-from-of']])
:- f_u_ob_c36_decl_inverses(u_linked_from, u_linked_from_of, _Ignored4).
/*
(defun ol-get (ob link-type dir context)
  (let ((links (ob$gets ob (if (eq? dir 'backward)
                                'linked-to-of
                                'linked-from-of)))
        (other-dir (if (eq? dir 'backward) 'linked-from
                                           'linked-to)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append (ob$gets link other-dir) result))))
           (yresult result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20906 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ol-get',[ob,'link-type',dir,context],[let,[[links,['ob$gets',ob,[if,['eq?',dir,[quote,backward]],[quote,'linked-to-of'],[quote,'linked-from-of']]]],['other-dir',[if,['eq?',dir,[quote,backward]],[quote,'linked-from'],[quote,'linked-to']]]],[yloop,[initial,[result,[]]],[yfor,link,in,links],[ydo,[if,[and,['cx$true?',context,link],['ty$instance-of?',link,'link-type']],[setq,result,[append,['ob$gets',link,'other-dir'],result]]]],[yresult,result]]]])
wl:lambda_def(defun, u_ol_get, f_u_ol_get, [u_ob, u_link_type, ext_dir, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [if, [u_eq_c63, ext_dir, [quote, u_backward]], [quote, u_linked_to_of], [quote, u_linked_from_of]]]], [u_other_dir, [if, [u_eq_c63, ext_dir, [quote, u_backward]], [quote, u_linked_from], [quote, u_linked_to]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_ol_get, f_u_ol_get, [u_ob, u_link_type, ext_dir, u_context], arginfo{all:[u_ob, u_link_type, ext_dir, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, ext_dir, u_context], opt:0, req:[u_ob, u_link_type, ext_dir, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ol_get).

/*

### Compiled:  `U::OL-GET` 
*/
f_u_ol_get(Ob, Link_type, Ext_dir, Context, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_ob, Ob), bv(u_link_type, Link_type), bv(ext_dir, Ext_dir), bv(u_context, Context)|Env],
	get_var(Env19, u_ob, Ob_Get),
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST),
	(   IFTEST\==[]
	->  _276862008=u_linked_to_of
	;   _276862008=u_linked_from_of
	),
	f_u_ob_c36_gets(Ob_Get, _276862008, Links_Init),
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST13),
	(   IFTEST13\==[]
	->  Other_dir_Init=u_linked_from
	;   Other_dir_Init=u_linked_to
	),
	LEnv=[bv(u_links, Links_Init), bv(u_other_dir, Other_dir_Init)|Env19],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  LetResult),
	LetResult=FnResult.
:- set_opv(f_u_ol_get, classof, claz_function),
   set_opv(u_ol_get, compile_as, kw_function),
   set_opv(u_ol_get, function, f_u_ol_get),
   _Ignored4=u_ol_get.
/*
:- side_effect(assert_lsp(u_ol_get,
			  wl:lambda_def(defun, u_ol_get, f_u_ol_get, [u_ob, u_link_type, ext_dir, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [if, [u_eq_c63, ext_dir, [quote, u_backward]], [quote, u_linked_to_of], [quote, u_linked_from_of]]]], [u_other_dir, [if, [u_eq_c63, ext_dir, [quote, u_backward]], [quote, u_linked_from], [quote, u_linked_to]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ol_get,
			  wl:arglist_info(u_ol_get, f_u_ol_get, [u_ob, u_link_type, ext_dir, u_context], arginfo{all:[u_ob, u_link_type, ext_dir, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, ext_dir, u_context], opt:0, req:[u_ob, u_link_type, ext_dir, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ol_get, wl:init_args(exact_only, f_u_ol_get))).
*/
/*
(defun ol-set (from-ob link-type to-ob context)
  (cx$assert context (ob$fcreate `((quote ,link-type)
                                   linked-from ,from-ob
                                   linked-to ,to-ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21491 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ol-set',['from-ob','link-type','to-ob',context],['cx$assert',context,['ob$fcreate',['#BQ',[[quote,['#COMMA','link-type']],'linked-from',['#COMMA','from-ob'],'linked-to',['#COMMA','to-ob']]]]]])
wl:lambda_def(defun, u_ol_set, f_u_ol_set, [u_from_ob, u_link_type, u_to_ob, u_context], [[u_cx_c36_assert, u_context, [u_ob_c36_fcreate, ['#BQ', [[quote, ['#COMMA', u_link_type]], u_linked_from, ['#COMMA', u_from_ob], u_linked_to, ['#COMMA', u_to_ob]]]]]]).
wl:arglist_info(u_ol_set, f_u_ol_set, [u_from_ob, u_link_type, u_to_ob, u_context], arginfo{all:[u_from_ob, u_link_type, u_to_ob, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from_ob, u_link_type, u_to_ob, u_context], opt:0, req:[u_from_ob, u_link_type, u_to_ob, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ol_set).

/*

### Compiled:  `U::OL-SET` 
*/
f_u_ol_set(From_ob, Link_type, To_ob, Context, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_from_ob, From_ob), bv(u_link_type, Link_type), bv(u_to_ob, To_ob), bv(u_context, Context)|Env],
	get_var(Env10, u_context, Context_Get),
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ [quote, ['#COMMA', u_link_type]],
			       u_linked_from,
			       ['#COMMA', u_from_ob],
			       u_linked_to,
			       ['#COMMA', u_to_ob]
			     ]
			   ],
			   C36_fcreate_Ret),
	f_u_cx_c36_assert(Context_Get, C36_fcreate_Ret, C36_assert_Ret),
	C36_assert_Ret=FnResult.
:- set_opv(f_u_ol_set, classof, claz_function),
   set_opv(u_ol_set, compile_as, kw_function),
   set_opv(u_ol_set, function, f_u_ol_set),
   _Ignored4=u_ol_set.
/*
:- side_effect(assert_lsp(u_ol_set,
			  wl:lambda_def(defun, u_ol_set, f_u_ol_set, [u_from_ob, u_link_type, u_to_ob, u_context], [[u_cx_c36_assert, u_context, [u_ob_c36_fcreate, ['#BQ', [[quote, ['#COMMA', u_link_type]], u_linked_from, ['#COMMA', u_from_ob], u_linked_to, ['#COMMA', u_to_ob]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ol_set,
			  wl:arglist_info(u_ol_set, f_u_ol_set, [u_from_ob, u_link_type, u_to_ob, u_context], arginfo{all:[u_from_ob, u_link_type, u_to_ob, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from_ob, u_link_type, u_to_ob, u_context], opt:0, req:[u_from_ob, u_link_type, u_to_ob, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ol_set, wl:init_args(exact_only, f_u_ol_set))).
*/
/*
(defun has-link? (ob direction type context)
  (let ((links (ob$gets ob direction)))
    (any? (lambda (x) (and (cx$true? context x)
                           (ty$instance-of? x type))) links)))

; Returns forward linked obs (e.g., results)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21706 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'has-link?',[ob,direction,type,context],[let,[[links,['ob$gets',ob,direction]]],['any?',[lambda,[x],[and,['cx$true?',context,x],['ty$instance-of?',x,type]]],links]]])
wl:lambda_def(defun, u_has_link_c63, f_u_has_link_c63, [u_ob, u_direction, type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, u_direction]]], [u_any_c63, [lambda, [u_x], [and, [u_cx_c36_true_c63, u_context, u_x], [u_ty_c36_instance_of_c63, u_x, type]]], u_links]]]).
wl:arglist_info(u_has_link_c63, f_u_has_link_c63, [u_ob, u_direction, type, u_context], arginfo{all:[u_ob, u_direction, type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_direction, type, u_context], opt:0, req:[u_ob, u_direction, type, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_has_link_c63).

/*

### Compiled:  `U::HAS-LINK?` 
*/
f_u_has_link_c63(Ob, Direction, Type, Context, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_ob, Ob), bv(u_direction, Direction), bv(type, Type), bv(u_context, Context)|Env],
	get_var(Env15, u_direction, Direction_Get),
	get_var(Env15, u_ob, Ob_Get),
	f_u_ob_c36_gets(Ob_Get, Direction_Get, Links_Init),
	LEnv=[bv(u_links, Links_Init)|Env15],
	f_u_any_c63(
		    [ lambda,
		      [u_x],
		      
		      [ and,
			[u_cx_c36_true_c63, u_context, u_x],
			[u_ty_c36_instance_of_c63, u_x, type]
		      ]
		    ],
		    u_links,
		    LetResult),
	LetResult=FnResult.
:- set_opv(f_u_has_link_c63, classof, claz_function),
   set_opv(u_has_link_c63, compile_as, kw_function),
   set_opv(u_has_link_c63, function, f_u_has_link_c63),
   _Ignored4=u_has_link_c63.
/*
:- side_effect(assert_lsp(u_has_link_c63,
			  wl:lambda_def(defun, u_has_link_c63, f_u_has_link_c63, [u_ob, u_direction, type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, u_direction]]], [u_any_c63, [lambda, [u_x], [and, [u_cx_c36_true_c63, u_context, u_x], [u_ty_c36_instance_of_c63, u_x, type]]], u_links]]]))).
*/
/*
:- side_effect(assert_lsp(u_has_link_c63,
			  wl:arglist_info(u_has_link_c63, f_u_has_link_c63, [u_ob, u_direction, type, u_context], arginfo{all:[u_ob, u_direction, type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_direction, type, u_context], opt:0, req:[u_ob, u_direction, type, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_has_link_c63,
			  wl:init_args(exact_only, f_u_has_link_c63))).
*/
/*
 Returns forward linked obs (e.g., results)
*/
/*
(defun get-links (ob link-type context)
  (let ((links (ob$gets ob 'linked-from-of)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append! result (list link)))))
           (yresult result))))

; Returns backward linked obs (e.g., causes)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21948 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-links',[ob,'link-type',context],[let,[[links,['ob$gets',ob,[quote,'linked-from-of']]]],[yloop,[initial,[result,[]]],[yfor,link,in,links],[ydo,[if,[and,['cx$true?',context,link],['ty$instance-of?',link,'link-type']],[setq,result,['append!',result,[list,link]]]]],[yresult,result]]]])
wl:lambda_def(defun, u_get_links, f_u_get_links, [u_ob, u_link_type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_from_of]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [u_append_c33, u_result, [list, u_link]]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_get_links, f_u_get_links, [u_ob, u_link_type, u_context], arginfo{all:[u_ob, u_link_type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, u_context], opt:0, req:[u_ob, u_link_type, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_links).

/*

### Compiled:  `U::GET-LINKS` 
*/
f_u_get_links(Ob, Link_type, Context, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_ob, Ob), bv(u_link_type, Link_type), bv(u_context, Context)|Env],
	get_var(Env14, u_ob, Ob_Get),
	f_u_ob_c36_gets(Ob_Get, u_linked_from_of, Links_Init),
	LEnv=[bv(u_links, Links_Init)|Env14],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  LetResult),
	LetResult=FnResult.
:- set_opv(f_u_get_links, classof, claz_function),
   set_opv(u_get_links, compile_as, kw_function),
   set_opv(u_get_links, function, f_u_get_links),
   _Ignored4=u_get_links.
/*
:- side_effect(assert_lsp(u_get_links,
			  wl:lambda_def(defun, u_get_links, f_u_get_links, [u_ob, u_link_type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_from_of]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [u_append_c33, u_result, [list, u_link]]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_get_links,
			  wl:arglist_info(u_get_links, f_u_get_links, [u_ob, u_link_type, u_context], arginfo{all:[u_ob, u_link_type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, u_context], opt:0, req:[u_ob, u_link_type, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_links, wl:init_args(exact_only, f_u_get_links))).
*/
/*
 Returns backward linked obs (e.g., causes)
*/
/*
(defun get-links-from (ob link-type context)
  (let ((links (ob$gets ob 'linked-to-of)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append! result (list link)))))
           (yresult result))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:22350 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-links-from',[ob,'link-type',context],[let,[[links,['ob$gets',ob,[quote,'linked-to-of']]]],[yloop,[initial,[result,[]]],[yfor,link,in,links],[ydo,[if,[and,['cx$true?',context,link],['ty$instance-of?',link,'link-type']],[setq,result,['append!',result,[list,link]]]]],[yresult,result]]]])
wl:lambda_def(defun, u_get_links_from, f_u_get_links_from, [u_ob, u_link_type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_to_of]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [u_append_c33, u_result, [list, u_link]]]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_get_links_from, f_u_get_links_from, [u_ob, u_link_type, u_context], arginfo{all:[u_ob, u_link_type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, u_context], opt:0, req:[u_ob, u_link_type, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_links_from).

/*

### Compiled:  `U::GET-LINKS-FROM` 
*/
f_u_get_links_from(Ob, Link_type, Context, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_ob, Ob), bv(u_link_type, Link_type), bv(u_context, Context)|Env],
	get_var(Env14, u_ob, Ob_Get),
	f_u_ob_c36_gets(Ob_Get, u_linked_to_of, Links_Init),
	LEnv=[bv(u_links, Links_Init)|Env14],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  LetResult),
	LetResult=FnResult.
:- set_opv(f_u_get_links_from, classof, claz_function),
   set_opv(u_get_links_from, compile_as, kw_function),
   set_opv(u_get_links_from, function, f_u_get_links_from),
   _Ignored4=u_get_links_from.
/*
:- side_effect(assert_lsp(u_get_links_from,
			  wl:lambda_def(defun, u_get_links_from, f_u_get_links_from, [u_ob, u_link_type, u_context], [[let, [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_to_of]]]], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_link, u_in, u_links], [u_ydo, [if, [and, [u_cx_c36_true_c63, u_context, u_link], [u_ty_c36_instance_of_c63, u_link, u_link_type]], [setq, u_result, [u_append_c33, u_result, [list, u_link]]]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_get_links_from,
			  wl:arglist_info(u_get_links_from, f_u_get_links_from, [u_ob, u_link_type, u_context], arginfo{all:[u_ob, u_link_type, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_link_type, u_context], opt:0, req:[u_ob, u_link_type, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_links_from,
			  wl:init_args(exact_only, f_u_get_links_from))).
*/
/*
(defun ol-path (ob1 ob2 link-type dir
                      context predicate bindings)
  (yloop (initial
         (result nil)
         (count 0)
         (next-obs (ol-get ob1 link-type dir context)))
        (yuntil
         (or result
             (if (> count *max-breadth*)
                 (progn
                  (ndbg *gate-dbg* ob-warn
                        "Exceeded max breadth in ob-link-path."(defun ol-path (ob1 ob2 link-type dir\n                      context predicate bindings)\n  (yloop (initial\n         (result nil)\n         (count 0)\n         (next-obs (ol-get ob1 link-type dir context)))\n        (yuntil\n         (or result\n             (if (> count *max-breadth*)\n                 (progn\n                  (ndbg *gate-dbg* ob-warn\n                        \"Exceeded max breadth in ob-link-path.~%\")\n                  t)\n                 nil)))\n        (ywhile next-obs)\n        (ydo\n         (yloop (yfor next-ob in next-obs)\n               (yuntil result)\n               (ydo (setq result\n                        (if (procedure? predicate)\n                            (funcall predicate ob2 next-ob)\n                            (ob$unify-cx ob2 next-ob bindings context)))\n                   (if result (setq result (cons next-ob (cdr result))))))\n         (if (null? result)\n             (setq next-obs\n                  (walk-append\n                   (lambda (ob) (ol-get ob link-type dir context))\n                   next-obs)))\n         (increment-me count))\n        (yresult result)))\n\n; ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:22710 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ol-path',[ob1,ob2,'link-type',dir,context,predicate,bindings],[yloop,[initial,[result,[]],[count,0],['next-obs',['ol-get',ob1,'link-type',dir,context]]],[yuntil,[or,result,[if,[>,count,'*max-breadth*'],[progn,[ndbg,'*gate-dbg*','ob-warn','$STRING'("Exceeded max breadth in ob-link-path.~%")],t],[]]]],[ywhile,'next-obs'],[ydo,[yloop,[yfor,'next-ob',in,'next-obs'],[yuntil,result],[ydo,[setq,result,[if,['procedure?',predicate],[funcall,predicate,ob2,'next-ob'],['ob$unify-cx',ob2,'next-ob',bindings,context]]],[if,result,[setq,result,[cons,'next-ob',[cdr,result]]]]]],[if,['null?',result],[setq,'next-obs',['walk-append',[lambda,[ob],['ol-get',ob,'link-type',dir,context]],'next-obs']]],['increment-me',count]],[yresult,result]]])
wl:lambda_def(defun, u_ol_path, f_u_ol_path, [u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], [[u_yloop, [u_initial, [u_result, []], [count, 0], [u_next_obs, [u_ol_get, u_ob1, u_link_type, ext_dir, u_context]]], [u_yuntil, [or, u_result, [if, [>, count, u_xx_max_breadth_xx], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Exceeded max breadth in ob-link-path.~%")], t], []]]], [u_ywhile, u_next_obs], [u_ydo, [u_yloop, [u_yfor, u_next_ob, u_in, u_next_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [if, [u_procedure_c63, predicate], [funcall, predicate, u_ob2, u_next_ob], [u_ob_c36_unify_cx, u_ob2, u_next_ob, bindings, u_context]]], [if, u_result, [setq, u_result, [cons, u_next_ob, [cdr, u_result]]]]]], [if, [u_null_c63, u_result], [setq, u_next_obs, [u_walk_append, [lambda, [u_ob], [u_ol_get, u_ob, u_link_type, ext_dir, u_context]], u_next_obs]]], [u_increment_me, count]], [u_yresult, u_result]]]).
wl:arglist_info(u_ol_path, f_u_ol_path, [u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], arginfo{all:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], opt:0, req:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ol_path).

/*

### Compiled:  `U::OL-PATH` 
*/
f_u_ol_path(Ob1, Ob2, Link_type, Ext_dir, Context, Predicate, Bindings, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(u_link_type, Link_type), bv(ext_dir, Ext_dir), bv(u_context, Context), bv(predicate, Predicate), bv(bindings, Bindings)|Env],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      [count, 0],
		      
		      [ u_next_obs,
			[u_ol_get, u_ob1, u_link_type, ext_dir, u_context]
		      ]
		    ],
		    
		    [ u_yuntil,
		      
		      [ or,
			u_result,
			
			[ if,
			  [>, count, u_xx_max_breadth_xx],
			  
			  [ progn,
			    
			    [ u_ndbg,
			      u_xx_gate_dbg_xx,
			      u_ob_warn,
			      '$ARRAY'([*],
				       claz_base_character,
				       "Exceeded max breadth in ob-link-path.~%")
			    ],
			    t
			  ],
			  []
			]
		      ]
		    ],
		    [u_ywhile, u_next_obs],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_next_ob, u_in, u_next_obs],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ if,
			      [u_procedure_c63, predicate],
			      [funcall, predicate, u_ob2, u_next_ob],
			      
			      [ u_ob_c36_unify_cx,
				u_ob2,
				u_next_ob,
				bindings,
				u_context
			      ]
			    ]
			  ],
			  
			  [ if,
			    u_result,
			    [setq, u_result, [cons, u_next_ob, [cdr, u_result]]]
			  ]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ setq,
			  u_next_obs,
			  
			  [ u_walk_append,
			    
			    [ lambda,
			      [u_ob],
			      [u_ol_get, u_ob, u_link_type, ext_dir, u_context]
			    ],
			    u_next_obs
			  ]
			]
		      ],
		      [u_increment_me, count]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ol_path, classof, claz_function),
   set_opv(u_ol_path, compile_as, kw_function),
   set_opv(u_ol_path, function, f_u_ol_path),
   _Ignored4=u_ol_path.
/*
:- side_effect(assert_lsp(u_ol_path,
			  wl:lambda_def(defun, u_ol_path, f_u_ol_path, [u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], [[u_yloop, [u_initial, [u_result, []], [count, 0], [u_next_obs, [u_ol_get, u_ob1, u_link_type, ext_dir, u_context]]], [u_yuntil, [or, u_result, [if, [>, count, u_xx_max_breadth_xx], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Exceeded max breadth in ob-link-path.~%")], t], []]]], [u_ywhile, u_next_obs], [u_ydo, [u_yloop, [u_yfor, u_next_ob, u_in, u_next_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [if, [u_procedure_c63, predicate], [funcall, predicate, u_ob2, u_next_ob], [u_ob_c36_unify_cx, u_ob2, u_next_ob, bindings, u_context]]], [if, u_result, [setq, u_result, [cons, u_next_ob, [cdr, u_result]]]]]], [if, [u_null_c63, u_result], [setq, u_next_obs, [u_walk_append, [lambda, [u_ob], [u_ol_get, u_ob, u_link_type, ext_dir, u_context]], u_next_obs]]], [u_increment_me, count]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ol_path,
			  wl:arglist_info(u_ol_path, f_u_ol_path, [u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], arginfo{all:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], opt:0, req:[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ol_path, wl:init_args(exact_only, f_u_ol_path))).
*/
/*
 End of file.
*/


%; Total compilation time: 13.743 seconds


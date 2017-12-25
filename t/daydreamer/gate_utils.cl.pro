#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_utils" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:15:27 2017

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
 10/13/84: A few utility functions and other initialization
*/
/*
   1/6/86: Added new variable syntax
*/
/*
  9/27/86: Removed flavors
*/
/*
*/
/*
*******************************************************************************
*/
/*
(defun make-typed-var (name)
  (cond
   ((eq? name *question-mark-atom*)
    (make-var nil nil))
   ((eq? name 'self)
    (make-var name *person-ob*))
   ((eq? name 'other)
    (make-var name *person-ob*))
   (else (let* ((str (symbol->string name))
                (len (string-length str))
                (last-char (nthchar str (- len 1))))
               (if (digit? last-char 10)
               (make-var name 
                         (ob$name->ob
                          (string->symbol
                           (string-slice str 0 (- len 1)))))
                   (make-var name (ob$name->ob name)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:411 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'make-typed-var',[name],[cond,[['eq?',name,'*question-mark-atom*'],['make-var',[],[]]],[['eq?',name,[quote,self]],['make-var',name,'*person-ob*']],[['eq?',name,[quote,other]],['make-var',name,'*person-ob*']],[else,['let*',[[str,['symbol->string',name]],[len,['string-length',str]],['last-char',[nthchar,str,[-,len,1]]]],[if,['digit?','last-char',10],['make-var',name,['ob$name->ob',['string->symbol',['string-slice',str,0,[-,len,1]]]]],['make-var',name,['ob$name->ob',name]]]]]]])
wl:lambda_def(defun, u_make_typed_var, f_u_make_typed_var, [sys_name], [[cond, [[u_eq_c63, sys_name, u_xx_question_mark_atom_xx], [u_make_var, [], []]], [[u_eq_c63, sys_name, [quote, u_self]], [u_make_var, sys_name, u_xx_person_ob_xx]], [[u_eq_c63, sys_name, [quote, u_other]], [u_make_var, sys_name, u_xx_person_ob_xx]], [u_else, [let_xx, [[u_str, [u_symbol_c62_string, sys_name]], [u_len, [u_string_length, u_str]], [u_last_char, [u_nthchar, u_str, [-, u_len, 1]]]], [if, [u_digit_c63, u_last_char, 10], [u_make_var, sys_name, [u_ob_c36_name_c62_ob, [u_string_c62_symbol, [u_string_slice, u_str, 0, [-, u_len, 1]]]]], [u_make_var, sys_name, [u_ob_c36_name_c62_ob, sys_name]]]]]]]).
wl:arglist_info(u_make_typed_var, f_u_make_typed_var, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_make_typed_var).

/*

### Compiled:  `U::MAKE-TYPED-VAR` 
*/
f_u_make_typed_var(Name, LetResult) :-
	nop(global_env(Env)),
	Env43=[bv(sys_name, Name)|Env],
	f_u_eq_c63(sys_name, u_xx_question_mark_atom_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_make_var([], [], TrueResult39),
	    LetResult=TrueResult39
	;   f_u_eq_c63(sys_name, [quote, u_self], IFTEST9),
	    (   IFTEST9\==[]
	    ->  get_var(Env43, sys_name, Name_Get),
		get_var(Env43, u_xx_person_ob_xx, Xx_person_ob_xx_Get),
		f_u_make_var(Name_Get, Xx_person_ob_xx_Get, TrueResult37),
		LetResult=TrueResult37
	    ;   f_u_eq_c63(sys_name, [quote, u_other], IFTEST13),
		(   IFTEST13\==[]
		->  get_var(Env43, sys_name, Name_Get15),
		    get_var(Env43, u_xx_person_ob_xx, Xx_person_ob_xx_Get16),
		    f_u_make_var(Name_Get15,
				 Xx_person_ob_xx_Get16,
				 TrueResult35),
		    LetResult=TrueResult35
		;   get_var(Env43, u_else, IFTEST17),
		    (   IFTEST17\==[]
		    ->  f_u_symbol_c62_string(sys_name, Str_Init),
			f_u_string_length(u_str, Len_Init),
			f_u_nthchar(u_str, [-, u_len, 1], Last_char_Init),
			LEnv=[bv(u_str, Str_Init), bv(u_len, Len_Init), bv(u_last_char, Last_char_Init)|Env43],
			f_u_digit_c63(u_last_char, 10, IFTEST26),
			(   IFTEST26\==[]
			->  get_var(LEnv, sys_name, Name_Get28),
			    f_u_string_c62_symbol(
						  [ u_string_slice,
						    u_str,
						    0,
						    [-, u_len, 1]
						  ],
						  C62_ob_Param),
			    f_u_ob_c36_name_c62_ob(C62_ob_Param, C62_ob_Ret),
			    f_u_make_var(Name_Get28, C62_ob_Ret, TrueResult),
			    LetResult=TrueResult
			;   get_var(LEnv, sys_name, Name_Get29),
			    f_u_ob_c36_name_c62_ob(Name_Get29, C62_ob_Ret47),
			    f_u_make_var(Name_Get29, C62_ob_Ret47, ElseResult),
			    LetResult=ElseResult
			)
		    ;   LetResult=[]
		    )
		)
	    )
	).
:- set_opv(f_u_make_typed_var, classof, claz_function),
   set_opv(u_make_typed_var, compile_as, kw_function),
   set_opv(u_make_typed_var, function, f_u_make_typed_var),
   _Ignored4=u_make_typed_var.
/*
:- side_effect(assert_lsp(u_make_typed_var,
			  wl:lambda_def(defun, u_make_typed_var, f_u_make_typed_var, [sys_name], [[cond, [[u_eq_c63, sys_name, u_xx_question_mark_atom_xx], [u_make_var, [], []]], [[u_eq_c63, sys_name, [quote, u_self]], [u_make_var, sys_name, u_xx_person_ob_xx]], [[u_eq_c63, sys_name, [quote, u_other]], [u_make_var, sys_name, u_xx_person_ob_xx]], [u_else, [let_xx, [[u_str, [u_symbol_c62_string, sys_name]], [u_len, [u_string_length, u_str]], [u_last_char, [u_nthchar, u_str, [-, u_len, 1]]]], [if, [u_digit_c63, u_last_char, 10], [u_make_var, sys_name, [u_ob_c36_name_c62_ob, [u_string_c62_symbol, [u_string_slice, u_str, 0, [-, u_len, 1]]]]], [u_make_var, sys_name, [u_ob_c36_name_c62_ob, sys_name]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_make_typed_var,
			  wl:arglist_info(u_make_typed_var, f_u_make_typed_var, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_make_typed_var,
			  wl:init_args(exact_only, f_u_make_typed_var))).
*/
/*
(setq *bell-char* (ascii->char 7))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1030 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*bell-char*',['ascii->char',7]])
:- f_u_ascii_c62_char(7, _Ignored4),
   set_var(AEnv, u_xx_bell_char_xx, _Ignored4).
/*
(setq *esc-char* (ascii->char 27))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1065 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*esc-char*',['ascii->char',27]])
:- f_u_ascii_c62_char(27, _Ignored4),
   set_var(AEnv, u_xx_esc_char_xx, _Ignored4).
/*
(setq *del-char* (ascii->char 127))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1100 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*del-char*',['ascii->char',127]])
:- f_u_ascii_c62_char(127, _Ignored4),
   set_var(AEnv, u_xx_del_char_xx, _Ignored4).
/*
(setq *cr-char* (ascii->char 13))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1136 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*cr-char*',['ascii->char',13]])
:- f_u_ascii_c62_char(13, _Ignored4),
   set_var(AEnv, u_xx_cr_char_xx, _Ignored4).
/*
(setq *cntrl-z-char* (ascii->char 26))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1170 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*cntrl-z-char*',['ascii->char',26]])
:- f_u_ascii_c62_char(26, _Ignored4),
   set_var(AEnv, u_xx_cntrl_z_char_xx, _Ignored4).
/*
(setq *cntrl-rb-char* (ascii->char 29))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1209 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*cntrl-rb-char*',['ascii->char',29]])
:- f_u_ascii_c62_char(29, _Ignored4),
   set_var(AEnv, u_xx_cntrl_rb_char_xx, _Ignored4).
/*
(defun string-truncate (str len)
  (string-slice str 0 (min (string-length str) len)))

;
; NDBG: New Debugging Mechanism
;
; For use in the program:
;
; (ndbg-begin) - Start a new indentation level
; (ndbg-add-item rule) - Add item to list of current items
; (ndbg *dbg-stream* keyname "Message"(defun string-truncate (str len)\n  (string-slice str 0 (min (string-length str) len)))\n\n;\n; NDBG: New Debugging Mechanism\n;\n; For use in the program:\n;\n; (ndbg-begin) - Start a new indentation level\n; (ndbg-add-item rule) - Add item to list of current items\n; (ndbg *dbg-stream* keyname \"Message~%\") - Print a debugging message\n; (ndbg-remove-item rule) - Remove item to list of current items\n; (ndbg-end) - End indentation level\n;\n; For use at debugging time:\n;\n; (interest 'keyname . items) - Show debugging info for keyname\n;   when any item is present in current items, or if an item\n;   is 'all, always\n; (disinterest 'keyname . items) - Stop debugging info for keyname\n;   and items\n; (interests) - Show current interests\n; (ndbg-reset) - Reset indenting level back to zero\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'string-truncate',[str,len],['string-slice',str,0,[min,['string-length',str],len]]])
wl:lambda_def(defun, u_string_truncate, f_u_string_truncate, [u_str, u_len], [[u_string_slice, u_str, 0, [min, [u_string_length, u_str], u_len]]]).
wl:arglist_info(u_string_truncate, f_u_string_truncate, [u_str, u_len], arginfo{all:[u_str, u_len], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_str, u_len], opt:0, req:[u_str, u_len], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_truncate).

/*

### Compiled:  `U::STRING-TRUNCATE` 
*/
f_u_string_truncate(Str, Len, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_str, Str), bv(u_len, Len)|Env],
	f_u_string_slice(u_str,
			 0,
			 [min, [u_string_length, u_str], u_len],
			 String_slice_Ret),
	String_slice_Ret=FnResult.
:- set_opv(f_u_string_truncate, classof, claz_function),
   set_opv(u_string_truncate, compile_as, kw_function),
   set_opv(u_string_truncate, function, f_u_string_truncate),
   _Ignored4=u_string_truncate.
/*
:- side_effect(assert_lsp(u_string_truncate,
			  wl:lambda_def(defun, u_string_truncate, f_u_string_truncate, [u_str, u_len], [[u_string_slice, u_str, 0, [min, [u_string_length, u_str], u_len]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_truncate,
			  wl:arglist_info(u_string_truncate, f_u_string_truncate, [u_str, u_len], arginfo{all:[u_str, u_len], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_str, u_len], opt:0, req:[u_str, u_len], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_truncate,
			  wl:init_args(exact_only, f_u_string_truncate))).
*/
/*
*/
/*
 NDBG: New Debugging Mechanism
*/
/*
*/
/*
 For use in the program:
*/
/*
*/
/*
 (ndbg-begin) - Start a new indentation level
*/
/*
 (ndbg-add-item rule) - Add item to list of current items
*/
/*
 (ndbg *dbg-stream* keyname "Message" (ndbg *dbg-stream* keyname \"Message~%\") - Print a debugging message".
*/
/*
 (ndbg-remove-item rule) - Remove item to list of current items
*/
/*
 (ndbg-end) - End indentation level
*/
/*
*/
/*
 For use at debugging time:
*/
/*
*/
/*
 (interest 'keyname . items) - Show debugging info for keyname
*/
/*
   when any item is present in current items, or if an item
*/
/*
   is 'all, always
*/
/*
 (disinterest 'keyname . items) - Stop debugging info for keyname
*/
/*
   and items
*/
/*
 (interests) - Show current interests
*/
/*
 (ndbg-reset) - Reset indenting level back to zero
*/
/*
*/
/*
(setq *ndbg-interests* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2033 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ndbg-interests*',[]])
:- set_var(AEnv, setq, u_xx_ndbg_interests_xx, []).
/*
(setq *ndbg-level* 0)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2061 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ndbg-level*',0])
:- set_var(AEnv, setq, u_xx_ndbg_level_xx, 0).
/*
(setq *ndbg-items* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2083 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ndbg-items*',[]])
:- set_var(AEnv, setq, u_xx_ndbg_items_xx, []).
/*
(setq *ndbg-indentation* 1)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2107 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ndbg-indentation*',1])
:- set_var(AEnv, setq, u_xx_ndbg_indentation_xx, 1).
/*
(setq *ndbg-max-indentation* 50)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2135 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ndbg-max-indentation*',50])
:- set_var(AEnv, setq, u_xx_ndbg_max_indentation_xx, 50).
/*
(defun ndbg-add-item (item)
 (setq *ndbg-items* (cons item *ndbg-items*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2169 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-add-item',[item],[setq,'*ndbg-items*',[cons,item,'*ndbg-items*']]])
wl:lambda_def(defun, u_ndbg_add_item, f_u_ndbg_add_item, [item], [[setq, u_xx_ndbg_items_xx, [cons, item, u_xx_ndbg_items_xx]]]).
wl:arglist_info(u_ndbg_add_item, f_u_ndbg_add_item, [item], arginfo{all:[item], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[item], opt:0, req:[item], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_add_item).

/*

### Compiled:  `U::NDBG-ADD-ITEM` 
*/
f_u_ndbg_add_item(Item, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(item, Item)|Env],
	get_var(AEnv, item, Item_Get),
	get_var(AEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get),
	Xx_ndbg_items_xx=[Item_Get|Xx_ndbg_items_xx_Get],
	set_var(AEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	Xx_ndbg_items_xx=FnResult.
:- set_opv(f_u_ndbg_add_item, classof, claz_function),
   set_opv(u_ndbg_add_item, compile_as, kw_function),
   set_opv(u_ndbg_add_item, function, f_u_ndbg_add_item),
   _Ignored4=u_ndbg_add_item.
/*
:- side_effect(assert_lsp(u_ndbg_add_item,
			  wl:lambda_def(defun, u_ndbg_add_item, f_u_ndbg_add_item, [item], [[setq, u_xx_ndbg_items_xx, [cons, item, u_xx_ndbg_items_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_add_item,
			  wl:arglist_info(u_ndbg_add_item, f_u_ndbg_add_item, [item], arginfo{all:[item], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[item], opt:0, req:[item], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_add_item,
			  wl:init_args(exact_only, f_u_ndbg_add_item))).
*/
/*
(defun ndbg-remove-item (item)
 (setq *ndbg-items* (delq! item *ndbg-items*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2245 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-remove-item',[item],[setq,'*ndbg-items*',['delq!',item,'*ndbg-items*']]])
wl:lambda_def(defun, u_ndbg_remove_item, f_u_ndbg_remove_item, [item], [[setq, u_xx_ndbg_items_xx, [u_delq_c33, item, u_xx_ndbg_items_xx]]]).
wl:arglist_info(u_ndbg_remove_item, f_u_ndbg_remove_item, [item], arginfo{all:[item], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[item], opt:0, req:[item], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_remove_item).

/*

### Compiled:  `U::NDBG-REMOVE-ITEM` 
*/
f_u_ndbg_remove_item(Item, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(item, Item)|Env],
	f_u_delq_c33(item, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	set_var(AEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	Xx_ndbg_items_xx=FnResult.
:- set_opv(f_u_ndbg_remove_item, classof, claz_function),
   set_opv(u_ndbg_remove_item, compile_as, kw_function),
   set_opv(u_ndbg_remove_item, function, f_u_ndbg_remove_item),
   _Ignored4=u_ndbg_remove_item.
/*
:- side_effect(assert_lsp(u_ndbg_remove_item,
			  wl:lambda_def(defun, u_ndbg_remove_item, f_u_ndbg_remove_item, [item], [[setq, u_xx_ndbg_items_xx, [u_delq_c33, item, u_xx_ndbg_items_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_remove_item,
			  wl:arglist_info(u_ndbg_remove_item, f_u_ndbg_remove_item, [item], arginfo{all:[item], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[item], opt:0, req:[item], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_remove_item,
			  wl:init_args(exact_only, f_u_ndbg_remove_item))).
*/
/*
(defun ndbg-indentation (stream)
   (yloop (initial (cnt (min (* *ndbg-level* *ndbg-indentation*)
                            *ndbg-max-indentation*)))
         (ywhile (> cnt 0))
         (ydo (format stream " ")
             (setq cnt (- cnt 1)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2325 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-indentation',[stream],[yloop,[initial,[cnt,[min,[*,'*ndbg-level*','*ndbg-indentation*'],'*ndbg-max-indentation*']]],[ywhile,[>,cnt,0]],[ydo,[format,stream,'$STRING'(" ")],[setq,cnt,[-,cnt,1]]]]])
wl:lambda_def(defun, u_ndbg_indentation, f_u_ndbg_indentation, [stream], [[u_yloop, [u_initial, [u_cnt, [min, [*, u_xx_ndbg_level_xx, u_xx_ndbg_indentation_xx], u_xx_ndbg_max_indentation_xx]]], [u_ywhile, [>, u_cnt, 0]], [u_ydo, [format, stream, '$ARRAY'([*], claz_base_character, " ")], [setq, u_cnt, [-, u_cnt, 1]]]]]).
wl:arglist_info(u_ndbg_indentation, f_u_ndbg_indentation, [stream], arginfo{all:[stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream], opt:0, req:[stream], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_indentation).

/*

### Compiled:  `U::NDBG-INDENTATION` 
*/
f_u_ndbg_indentation(Stream, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(stream, Stream)|Env],
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
		      [format, stream, '$ARRAY'([*], claz_base_character, " ")],
		      [setq, u_cnt, [-, u_cnt, 1]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ndbg_indentation, classof, claz_function),
   set_opv(u_ndbg_indentation, compile_as, kw_function),
   set_opv(u_ndbg_indentation, function, f_u_ndbg_indentation),
   _Ignored4=u_ndbg_indentation.
/*
:- side_effect(assert_lsp(u_ndbg_indentation,
			  wl:lambda_def(defun, u_ndbg_indentation, f_u_ndbg_indentation, [stream], [[u_yloop, [u_initial, [u_cnt, [min, [*, u_xx_ndbg_level_xx, u_xx_ndbg_indentation_xx], u_xx_ndbg_max_indentation_xx]]], [u_ywhile, [>, u_cnt, 0]], [u_ydo, [format, stream, '$ARRAY'([*], claz_base_character, " ")], [setq, u_cnt, [-, u_cnt, 1]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_indentation,
			  wl:arglist_info(u_ndbg_indentation, f_u_ndbg_indentation, [stream], arginfo{all:[stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream], opt:0, req:[stream], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_indentation,
			  wl:init_args(exact_only, f_u_ndbg_indentation))).
*/
/*
(defun ndbg-begin ()
  (setq *ndbg-level* (+ *ndbg-level* 1)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2577 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-begin',[],[setq,'*ndbg-level*',[+,'*ndbg-level*',1]]])
wl:lambda_def(defun, u_ndbg_begin, f_u_ndbg_begin, [], [[setq, u_xx_ndbg_level_xx, [+, u_xx_ndbg_level_xx, 1]]]).
wl:arglist_info(u_ndbg_begin, f_u_ndbg_begin, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_begin).

/*

### Compiled:  `U::NDBG-BEGIN` 
*/
f_u_ndbg_begin(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	get_var(AEnv, u_xx_ndbg_level_xx, Xx_ndbg_level_xx_Get),
	+(Xx_ndbg_level_xx_Get, 1, Xx_ndbg_level_xx),
	set_var(AEnv, u_xx_ndbg_level_xx, Xx_ndbg_level_xx),
	Xx_ndbg_level_xx=FnResult.
:- set_opv(f_u_ndbg_begin, classof, claz_function),
   set_opv(u_ndbg_begin, compile_as, kw_function),
   set_opv(u_ndbg_begin, function, f_u_ndbg_begin),
   _Ignored4=u_ndbg_begin.
/*
:- side_effect(assert_lsp(u_ndbg_begin,
			  wl:lambda_def(defun, u_ndbg_begin, f_u_ndbg_begin, [], [[setq, u_xx_ndbg_level_xx, [+, u_xx_ndbg_level_xx, 1]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_begin,
			  wl:arglist_info(u_ndbg_begin, f_u_ndbg_begin, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_begin,
			  wl:init_args(exact_only, f_u_ndbg_begin))).
*/
/*
(defun ndbg-end ()
  (setq *ndbg-level* (- *ndbg-level* 1)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2641 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-end',[],[setq,'*ndbg-level*',[-,'*ndbg-level*',1]]])
wl:lambda_def(defun, u_ndbg_end, f_u_ndbg_end, [], [[setq, u_xx_ndbg_level_xx, [-, u_xx_ndbg_level_xx, 1]]]).
wl:arglist_info(u_ndbg_end, f_u_ndbg_end, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_end).

/*

### Compiled:  `U::NDBG-END` 
*/
f_u_ndbg_end(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	get_var(AEnv, u_xx_ndbg_level_xx, Xx_ndbg_level_xx_Get),
	-(Xx_ndbg_level_xx_Get, 1, Xx_ndbg_level_xx),
	set_var(AEnv, u_xx_ndbg_level_xx, Xx_ndbg_level_xx),
	Xx_ndbg_level_xx=FnResult.
:- set_opv(f_u_ndbg_end, classof, claz_function),
   set_opv(u_ndbg_end, compile_as, kw_function),
   set_opv(u_ndbg_end, function, f_u_ndbg_end),
   _Ignored4=u_ndbg_end.
/*
:- side_effect(assert_lsp(u_ndbg_end,
			  wl:lambda_def(defun, u_ndbg_end, f_u_ndbg_end, [], [[setq, u_xx_ndbg_level_xx, [-, u_xx_ndbg_level_xx, 1]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_end,
			  wl:arglist_info(u_ndbg_end, f_u_ndbg_end, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_end, wl:init_args(exact_only, f_u_ndbg_end))).
*/
/*
(defun ndbg-reset () (setq *ndbg-level* 0))

; Use (interest 'unify ^rule) and (interest 'show ^rule)
; to get full debugging info for a rule. (And use disinterest
; to turn off).

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2703 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ndbg-reset',[],[setq,'*ndbg-level*',0]])
wl:lambda_def(defun, u_ndbg_reset, f_u_ndbg_reset, [], [[setq, u_xx_ndbg_level_xx, 0]]).
wl:arglist_info(u_ndbg_reset, f_u_ndbg_reset, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_reset).

/*

### Compiled:  `U::NDBG-RESET` 
*/
f_u_ndbg_reset(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	set_var(AEnv, setq, u_xx_ndbg_level_xx, 0),
	0=FnResult.
:- set_opv(f_u_ndbg_reset, classof, claz_function),
   set_opv(u_ndbg_reset, compile_as, kw_function),
   set_opv(u_ndbg_reset, function, f_u_ndbg_reset),
   _Ignored4=u_ndbg_reset.
/*
:- side_effect(assert_lsp(u_ndbg_reset,
			  wl:lambda_def(defun, u_ndbg_reset, f_u_ndbg_reset, [], [[setq, u_xx_ndbg_level_xx, 0]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_reset,
			  wl:arglist_info(u_ndbg_reset, f_u_ndbg_reset, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_reset,
			  wl:init_args(exact_only, f_u_ndbg_reset))).
*/
/*
 Use (interest 'unify ^rule) and (interest 'show ^rule)
*/
/*
 to get full debugging info for a rule. (And use disinterest
*/
/*
 to turn off).
*/
/*
(defun interest (key &rest items)
  (let ((found (assq key *ndbg-interests*)))
    (if found
        (yloop (yfor item in items)
              (ydo (if (memq? item (cdr found))
                       (format *gate-output*
                               "Item "(defun interest (key &rest items)\n  (let ((found (assq key *ndbg-interests*)))\n    (if found\n        (yloop (yfor item in items)\n              (ydo (if (memq? item (cdr found))\n                       (format *gate-output*\n                               \"Item ~A key ~A already an interest~%\"\n                               item key)\n                       (setf (cdr found) (cons item (cdr found))))))\n        (setq *ndbg-interests* (cons (cons key items) *ndbg-interests*)))\n    (interests)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2884 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,interest,[key,'&rest',items],[let,[[found,[assq,key,'*ndbg-interests*']]],[if,found,[yloop,[yfor,item,in,items],[ydo,[if,['memq?',item,[cdr,found]],[format,'*gate-output*','$STRING'("Item ~A key ~A already an interest~%"),item,key],[setf,[cdr,found],[cons,item,[cdr,found]]]]]],[setq,'*ndbg-interests*',[cons,[cons,key,items],'*ndbg-interests*']]],[interests]]])
wl:lambda_def(defun, u_interest, f_u_interest, [key, c38_rest, u_items], [[let, [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]], [if, u_found, [u_yloop, [u_yfor, item, u_in, u_items], [u_ydo, [if, [u_memq_c63, item, [cdr, u_found]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Item ~A key ~A already an interest~%"), item, key], [setf, [cdr, u_found], [cons, item, [cdr, u_found]]]]]], [setq, u_xx_ndbg_interests_xx, [cons, [cons, key, u_items], u_xx_ndbg_interests_xx]]], [u_interests]]]).
wl:arglist_info(u_interest, f_u_interest, [key, c38_rest, u_items], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, u_items], opt:0, req:[key], rest:[u_items], sublists:0, whole:0}).
wl: init_args(1, f_u_interest).

/*

### Compiled:  `U::INTEREST` 
*/
f_u_interest(Key, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	CDR=[[[bv(key, Key), bv(u_items, RestNKeys)]|Env]|Env],
	f_ext_assq(key, u_xx_ndbg_interests_xx, Found_Init),
	LEnv=[bv(u_found, Found_Init)|CDR],
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
				       "Item ~A key ~A already an interest~%"),
			      item,
			      key
			    ],
			    [setf, [cdr, u_found], [cons, item, [cdr, u_found]]]
			  ]
			]
		      ],
		      TrueResult),
	    _382096310=TrueResult
	;   get_var(LEnv, key, Key_Get),
	    get_var(LEnv, u_items, Items_Get),
	    CAR=[Key_Get|Items_Get],
	    get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	    ElseResult=[CAR|Xx_ndbg_interests_xx_Get],
	    set_var(LEnv, u_xx_ndbg_interests_xx, ElseResult),
	    _382096310=ElseResult
	),
	f_u_interests(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_interest, classof, claz_function),
   set_opv(u_interest, compile_as, kw_function),
   set_opv(u_interest, function, f_u_interest),
   _Ignored4=u_interest.
/*
:- side_effect(assert_lsp(u_interest,
			  wl:lambda_def(defun, u_interest, f_u_interest, [key, c38_rest, u_items], [[let, [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]], [if, u_found, [u_yloop, [u_yfor, item, u_in, u_items], [u_ydo, [if, [u_memq_c63, item, [cdr, u_found]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Item ~A key ~A already an interest~%"), item, key], [setf, [cdr, u_found], [cons, item, [cdr, u_found]]]]]], [setq, u_xx_ndbg_interests_xx, [cons, [cons, key, u_items], u_xx_ndbg_interests_xx]]], [u_interests]]]))).
*/
/*
:- side_effect(assert_lsp(u_interest,
			  wl:arglist_info(u_interest, f_u_interest, [key, c38_rest, u_items], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, u_items], opt:0, req:[key], rest:[u_items], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_interest, wl:init_args(1, f_u_interest))).
*/
/*
(defun disinterest (key &rest items)
  (let ((found (assq key *ndbg-interests*)))
    (if found
        (yloop (yfor item in items)
               (ydo (if (not (memq? item (cdr found)))
                        (format *gate-output*
                                "Item "(defun disinterest (key &rest items)\n  (let ((found (assq key *ndbg-interests*)))\n    (if found\n        (yloop (yfor item in items)\n               (ydo (if (not (memq? item (cdr found)))\n                        (format *gate-output*\n                                \"Item ~A key ~A not an interest~%\"\n                                item key)\n                        (setf (cdr found) (delq! item (cdr found))))))\n        (format *gate-output* \"Key ~A not found at all~%\" key))\n    (interests)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3379 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,disinterest,[key,'&rest',items],[let,[[found,[assq,key,'*ndbg-interests*']]],[if,found,[yloop,[yfor,item,in,items],[ydo,[if,[not,['memq?',item,[cdr,found]]],[format,'*gate-output*','$STRING'("Item ~A key ~A not an interest~%"),item,key],[setf,[cdr,found],['delq!',item,[cdr,found]]]]]],[format,'*gate-output*','$STRING'("Key ~A not found at all~%"),key]],[interests]]])
wl:lambda_def(defun, u_disinterest, f_u_disinterest, [key, c38_rest, u_items], [[let, [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]], [if, u_found, [u_yloop, [u_yfor, item, u_in, u_items], [u_ydo, [if, [not, [u_memq_c63, item, [cdr, u_found]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Item ~A key ~A not an interest~%"), item, key], [setf, [cdr, u_found], [u_delq_c33, item, [cdr, u_found]]]]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Key ~A not found at all~%"), key]], [u_interests]]]).
wl:arglist_info(u_disinterest, f_u_disinterest, [key, c38_rest, u_items], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, u_items], opt:0, req:[key], rest:[u_items], sublists:0, whole:0}).
wl: init_args(1, f_u_disinterest).

/*

### Compiled:  `U::DISINTEREST` 
*/
f_u_disinterest(Key, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	CDR=[[[bv(key, Key), bv(u_items, RestNKeys)]|Env]|Env],
	f_ext_assq(key, u_xx_ndbg_interests_xx, Found_Init),
	LEnv=[bv(u_found, Found_Init)|CDR],
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
				       "Item ~A key ~A not an interest~%"),
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
	    _383617364=TrueResult
	;   get_var(LEnv, key, Key_Get),
	    get_var(LEnv, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 "Key ~A not found at all~%"),
			Key_Get
		      ],
		      ElseResult),
	    _383617364=ElseResult
	),
	f_u_interests(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_disinterest, classof, claz_function),
   set_opv(u_disinterest, compile_as, kw_function),
   set_opv(u_disinterest, function, f_u_disinterest),
   _Ignored4=u_disinterest.
/*
:- side_effect(assert_lsp(u_disinterest,
			  wl:lambda_def(defun, u_disinterest, f_u_disinterest, [key, c38_rest, u_items], [[let, [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]], [if, u_found, [u_yloop, [u_yfor, item, u_in, u_items], [u_ydo, [if, [not, [u_memq_c63, item, [cdr, u_found]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Item ~A key ~A not an interest~%"), item, key], [setf, [cdr, u_found], [u_delq_c33, item, [cdr, u_found]]]]]], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Key ~A not found at all~%"), key]], [u_interests]]]))).
*/
/*
:- side_effect(assert_lsp(u_disinterest,
			  wl:arglist_info(u_disinterest, f_u_disinterest, [key, c38_rest, u_items], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, u_items], opt:0, req:[key], rest:[u_items], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_disinterest, wl:init_args(1, f_u_disinterest))).
*/
/*
(defun interests () *ndbg-interests*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3875 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,interests,[],'*ndbg-interests*'])
wl:lambda_def(defun, u_interests, f_u_interests, [], [u_xx_ndbg_interests_xx]).
wl:arglist_info(u_interests, f_u_interests, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_interests).

/*

### Compiled:  `U::INTERESTS` 
*/
f_u_interests(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	Xx_ndbg_interests_xx_Get=FnResult.
:- set_opv(f_u_interests, classof, claz_function),
   set_opv(u_interests, compile_as, kw_function),
   set_opv(u_interests, function, f_u_interests),
   _Ignored4=u_interests.
/*
:- side_effect(assert_lsp(u_interests,
			  wl:lambda_def(defun, u_interests, f_u_interests, [], [u_xx_ndbg_interests_xx]))).
*/
/*
:- side_effect(assert_lsp(u_interests,
			  wl:arglist_info(u_interests, f_u_interests, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_interests, wl:init_args(exact_only, f_u_interests))).
*/
/*
(defun items () *ndbg-items*)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3913 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,items,[],'*ndbg-items*'])
wl:lambda_def(defun, u_items, f_u_items, [], [u_xx_ndbg_items_xx]).
wl:arglist_info(u_items, f_u_items, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_items).

/*

### Compiled:  `U::ITEMS` 
*/
f_u_items(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get),
	Xx_ndbg_items_xx_Get=FnResult.
:- set_opv(f_u_items, classof, claz_function),
   set_opv(u_items, compile_as, kw_function),
   set_opv(u_items, function, f_u_items),
   _Ignored4=u_items.
/*
:- side_effect(assert_lsp(u_items,
			  wl:lambda_def(defun, u_items, f_u_items, [], [u_xx_ndbg_items_xx]))).
*/
/*
:- side_effect(assert_lsp(u_items,
			  wl:arglist_info(u_items, f_u_items, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_items, wl:init_args(exact_only, f_u_items))).
*/
/*
(defun write-comments (comments stream)
  (let ((max-length (+ 2 (apply 'max
                                (map 'list string-length comments)))))
   (write-dashes-stream max-length stream)
   (yloop (yfor comment1 in comments)
         (ydo (dbg stream " "(defun write-comments (comments stream)\n  (let ((max-length (+ 2 (apply 'max\n                                (map 'list string-length comments)))))\n   (write-dashes-stream max-length stream)\n   (yloop (yfor comment1 in comments)\n         (ydo (dbg stream \" ~A~%\" comment1)))\n   (write-dashes-stream max-length stream)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3944 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'write-comments',[comments,stream],[let,[['max-length',[+,2,[apply,[quote,max],[map,[quote,list],'string-length',comments]]]]],['write-dashes-stream','max-length',stream],[yloop,[yfor,comment1,in,comments],[ydo,[dbg,stream,'$STRING'(" ~A~%"),comment1]]],['write-dashes-stream','max-length',stream]]])
wl:lambda_def(defun, u_write_comments, f_u_write_comments, [u_comments, stream], [[let, [[u_max_length, [+, 2, [apply, [quote, max], [map, [quote, list], u_string_length, u_comments]]]]], [u_write_dashes_stream, u_max_length, stream], [u_yloop, [u_yfor, u_comment1, u_in, u_comments], [u_ydo, [u_dbg, stream, '$ARRAY'([*], claz_base_character, " ~A~%"), u_comment1]]], [u_write_dashes_stream, u_max_length, stream]]]).
wl:arglist_info(u_write_comments, f_u_write_comments, [u_comments, stream], arginfo{all:[u_comments, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_comments, stream], opt:0, req:[u_comments, stream], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_write_comments).

/*

### Compiled:  `U::WRITE-COMMENTS` 
*/
f_u_write_comments(Comments, Stream, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_comments, Comments), bv(stream, Stream)|Env],
	get_var(Env19, u_comments, Comments_Get),
	get_var(Env19, u_string_length, String_length_Get),
	cl_map(list, String_length_Get, Comments_Get, KeysNRest),
	cl_apply(max, KeysNRest, Apply_Ret),
	+(2, Apply_Ret, Max_length_Init),
	LEnv=[bv(u_max_length, Max_length_Init)|Env19],
	get_var(LEnv, stream, Stream_Get),
	get_var(LEnv, u_max_length, Max_length_Get),
	f_u_write_dashes_stream(Max_length_Get, Stream_Get, Dashes_stream_Ret),
	f_u_yloop(
		  [ [u_yfor, u_comment1, u_in, u_comments],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			stream,
			'$ARRAY'([*], claz_base_character, " ~A~%"),
			u_comment1
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, stream, Stream_Get16),
	get_var(LEnv, u_max_length, Max_length_Get15),
	f_u_write_dashes_stream(Max_length_Get15, Stream_Get16, LetResult),
	LetResult=FnResult.
:- set_opv(f_u_write_comments, classof, claz_function),
   set_opv(u_write_comments, compile_as, kw_function),
   set_opv(u_write_comments, function, f_u_write_comments),
   _Ignored4=u_write_comments.
/*
:- side_effect(assert_lsp(u_write_comments,
			  wl:lambda_def(defun, u_write_comments, f_u_write_comments, [u_comments, stream], [[let, [[u_max_length, [+, 2, [apply, [quote, max], [map, [quote, list], u_string_length, u_comments]]]]], [u_write_dashes_stream, u_max_length, stream], [u_yloop, [u_yfor, u_comment1, u_in, u_comments], [u_ydo, [u_dbg, stream, '$ARRAY'([*], claz_base_character, " ~A~%"), u_comment1]]], [u_write_dashes_stream, u_max_length, stream]]]))).
*/
/*
:- side_effect(assert_lsp(u_write_comments,
			  wl:arglist_info(u_write_comments, f_u_write_comments, [u_comments, stream], arginfo{all:[u_comments, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_comments, stream], opt:0, req:[u_comments, stream], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_write_comments,
			  wl:init_args(exact_only, f_u_write_comments))).
*/
/*
(defun write-dashes-stream (number stream)
  (yloop (initial (count 1))
        (ywhile (<= count number))
        (ydo (format stream "-")
            (increment-me count)))
  (newline stream))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4265 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'write-dashes-stream',[number,stream],[yloop,[initial,[count,1]],[ywhile,[<=,count,number]],[ydo,[format,stream,'$STRING'("-")],['increment-me',count]]],[newline,stream]])
wl:lambda_def(defun, u_write_dashes_stream, f_u_write_dashes_stream, [number, stream], [[u_yloop, [u_initial, [count, 1]], [u_ywhile, [<=, count, number]], [u_ydo, [format, stream, '$ARRAY'([*], claz_base_character, "-")], [u_increment_me, count]]], [u_newline, stream]]).
wl:arglist_info(u_write_dashes_stream, f_u_write_dashes_stream, [number, stream], arginfo{all:[number, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[number, stream], opt:0, req:[number, stream], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_write_dashes_stream).

/*

### Compiled:  `U::WRITE-DASHES-STREAM` 
*/
f_u_write_dashes_stream(Number, Stream, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(number, Number), bv(stream, Stream)|Env],
	f_u_yloop(
		  [ [u_initial, [count, 1]],
		    [u_ywhile, [<=, count, number]],
		    
		    [ u_ydo,
		      [format, stream, '$ARRAY'([*], claz_base_character, "-")],
		      [u_increment_me, count]
		    ]
		  ],
		  Yloop_Ret),
	f_u_newline(stream, Newline_Ret),
	Newline_Ret=FnResult.
:- set_opv(f_u_write_dashes_stream, classof, claz_function),
   set_opv(u_write_dashes_stream, compile_as, kw_function),
   set_opv(u_write_dashes_stream, function, f_u_write_dashes_stream),
   _Ignored4=u_write_dashes_stream.
/*
:- side_effect(assert_lsp(u_write_dashes_stream,
			  wl:lambda_def(defun, u_write_dashes_stream, f_u_write_dashes_stream, [number, stream], [[u_yloop, [u_initial, [count, 1]], [u_ywhile, [<=, count, number]], [u_ydo, [format, stream, '$ARRAY'([*], claz_base_character, "-")], [u_increment_me, count]]], [u_newline, stream]]))).
*/
/*
:- side_effect(assert_lsp(u_write_dashes_stream,
			  wl:arglist_info(u_write_dashes_stream, f_u_write_dashes_stream, [number, stream], arginfo{all:[number, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[number, stream], opt:0, req:[number, stream], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_write_dashes_stream,
			  wl:init_args(exact_only, f_u_write_dashes_stream))).
*/
/*
(defun new-filename (atm)
 (let* ((name (string-downcase! (symbol->string (gen-id atm))))
        (filename (string-append "tmp." name)))
   (yloop
    (ywhile (file-exists? filename))
    (ydo
     (dbg *gate-warn-dbg* "-")
     (setq filename (string-append filename "a")))
    (yresult filename))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4461 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'new-filename',[atm],['let*',[[name,['string-downcase!',['symbol->string',['gen-id',atm]]]],[filename,['string-append','$STRING'("tmp."),name]]],[yloop,[ywhile,['file-exists?',filename]],[ydo,[dbg,'*gate-warn-dbg*','$STRING'("-")],[setq,filename,['string-append',filename,'$STRING'("a")]]],[yresult,filename]]]])
wl:lambda_def(defun, u_new_filename, f_u_new_filename, [u_atm], [[let_xx, [[sys_name, [u_string_downcase_c33, [u_symbol_c62_string, [u_gen_id, u_atm]]]], [u_filename, [u_string_append, '$ARRAY'([*], claz_base_character, "tmp."), sys_name]]], [u_yloop, [u_ywhile, [u_file_exists_c63, u_filename]], [u_ydo, [u_dbg, u_xx_gate_warn_dbg_xx, '$ARRAY'([*], claz_base_character, "-")], [setq, u_filename, [u_string_append, u_filename, '$ARRAY'([*], claz_base_character, "a")]]], [u_yresult, u_filename]]]]).
wl:arglist_info(u_new_filename, f_u_new_filename, [u_atm], arginfo{all:[u_atm], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_atm], opt:0, req:[u_atm], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_new_filename).

/*

### Compiled:  `U::NEW-FILENAME` 
*/
f_u_new_filename(Atm, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_atm, Atm)|Env],
	f_u_string_downcase_c33([u_symbol_c62_string, [u_gen_id, u_atm]],
				Name_Init),
	f_u_string_append(['$ARRAY'([*], claz_base_character, "tmp."), sys_name],
			  Filename_Init),
	LEnv=[bv(sys_name, Name_Init), bv(u_filename, Filename_Init)|Env14],
	f_u_yloop(
		  [ [u_ywhile, [u_file_exists_c63, u_filename]],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			u_xx_gate_warn_dbg_xx,
			'$ARRAY'([*], claz_base_character, "-")
		      ],
		      
		      [ setq,
			u_filename,
			
			[ u_string_append,
			  u_filename,
			  '$ARRAY'([*], claz_base_character, "a")
			]
		      ]
		    ],
		    [u_yresult, u_filename]
		  ],
		  LetResult),
	LetResult=FnResult.
:- set_opv(f_u_new_filename, classof, claz_function),
   set_opv(u_new_filename, compile_as, kw_function),
   set_opv(u_new_filename, function, f_u_new_filename),
   _Ignored4=u_new_filename.
/*
:- side_effect(assert_lsp(u_new_filename,
			  wl:lambda_def(defun, u_new_filename, f_u_new_filename, [u_atm], [[let_xx, [[sys_name, [u_string_downcase_c33, [u_symbol_c62_string, [u_gen_id, u_atm]]]], [u_filename, [u_string_append, '$ARRAY'([*], claz_base_character, "tmp."), sys_name]]], [u_yloop, [u_ywhile, [u_file_exists_c63, u_filename]], [u_ydo, [u_dbg, u_xx_gate_warn_dbg_xx, '$ARRAY'([*], claz_base_character, "-")], [setq, u_filename, [u_string_append, u_filename, '$ARRAY'([*], claz_base_character, "a")]]], [u_yresult, u_filename]]]]))).
*/
/*
:- side_effect(assert_lsp(u_new_filename,
			  wl:arglist_info(u_new_filename, f_u_new_filename, [u_atm], arginfo{all:[u_atm], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_atm], opt:0, req:[u_atm], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_new_filename,
			  wl:init_args(exact_only, f_u_new_filename))).
*/
/*
(set-macro-character #\?
   (lambda (stream ch)
     (let ((read-in (read stream t nil t))
           (colon-pos nil)
           (str nil))
        (setq str (symbol->string read-in))
        (cond
         ((setq colon-pos (string-posq #\+ str))
          (ob$fcreate
           `(UAND
             obj (UPROC
                  proc (QUOTE ,(string->symbol
                         (string-append (nthchdr str (1+ colon-pos)) "?"))))
             obj ,(make-typed-var
                   (string->symbol (substring str 0 colon-pos))))))
         ((setq colon-pos (string-posq #\: str))
          (if (= colon-pos 0)
              (make-var nil
               (ob$name->ob (string->symbol
                 (nthchdr str (1+ colon-pos))))) ; e.g. for ?:person
              (make-var (string->symbol (substring str 0 colon-pos))
                        (ob$name->ob
                         (string->symbol (nthchdr str (1+ colon-pos)))))))
         (else (make-typed-var read-in)))))
  t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4764 **********************/
:-lisp_compile_to_prolog(pkg_user,['set-macro-character',#\(?),[lambda,[stream,ch],[let,[['read-in',[read,stream,t,[],t]],['colon-pos',[]],[str,[]]],[setq,str,['symbol->string','read-in']],[cond,[[setq,'colon-pos',['string-posq',#\(+),str]],['ob$fcreate',['#BQ',['UAND',obj,['UPROC',proc,['QUOTE',['#COMMA',['string->symbol',['string-append',[nthchdr,str,['1+','colon-pos']],'$STRING'("?")]]]]],obj,['#COMMA',['make-typed-var',['string->symbol',[substring,str,0,'colon-pos']]]]]]]],[[setq,'colon-pos',['string-posq',#\(:),str]],[if,[=,'colon-pos',0],['make-var',[],['ob$name->ob',['string->symbol',[nthchdr,str,['1+','colon-pos']]]]],['make-var',['string->symbol',[substring,str,0,'colon-pos']],['ob$name->ob',['string->symbol',[nthchdr,str,['1+','colon-pos']]]]]]],[else,['make-typed-var','read-in']]]]],t])
:- Lambda=closure([ClosureEnvironment|CDR], LetResult, [stream, u_ch],  (get_var(ClosureEnvironment, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Read_in_Init), LEnv=[bv(u_read_in, Read_in_Init), bv(u_colon_pos, []), bv(u_str, [])|ClosureEnvironment], f_u_symbol_c62_string(u_read_in, Str), set_var(LEnv, u_str, Str), f_u_string_posq(#\(+), u_str, IFTEST), set_var(LEnv, u_colon_pos, IFTEST), (IFTEST\==[]->f_u_ob_c36_fcreate(['#BQ', [u_uand, u_obj, [u_uproc, u_proc, [quote, ['#COMMA', [u_string_c62_symbol, [u_string_append, [u_nthchdr, u_str, ['1+', u_colon_pos]], '$ARRAY'([*], claz_base_character, "?")]]]]], u_obj, ['#COMMA', [u_make_typed_var, [u_string_c62_symbol, [ext_substring, u_str, 0, u_colon_pos]]]]]], TrueResult29), LetResult=TrueResult29;f_u_string_posq(#\(:), u_str, IFTEST13), set_var(LEnv, u_colon_pos, IFTEST13), (IFTEST13\==[]->get_var(LEnv, u_colon_pos, Colon_pos_Get), (Colon_pos_Get=:=0->f_u_string_c62_symbol([u_nthchdr, u_str, ['1+', u_colon_pos]], C62_ob_Param), f_u_ob_c36_name_c62_ob(C62_ob_Param, C62_ob_Ret), f_u_make_var([], C62_ob_Ret, TrueResult), TrueResult27=TrueResult;f_u_string_c62_symbol([ext_substring, u_str, 0, u_colon_pos], Make_var_Param), f_u_string_c62_symbol([u_nthchdr, u_str, ['1+', u_colon_pos]], C62_ob_Param36), f_u_ob_c36_name_c62_ob(C62_ob_Param36, C62_ob_Ret40), f_u_make_var(Make_var_Param, C62_ob_Ret40, ElseResult), TrueResult27=ElseResult), ElseResult30=TrueResult27;get_var(LEnv, u_else, IFTEST21), (IFTEST21\==[]->get_var(LEnv, u_read_in, Read_in_Get), f_u_make_typed_var(Read_in_Get, TrueResult25), ElseResult28=TrueResult25;ElseResult26=[], ElseResult28=ElseResult26), ElseResult30=ElseResult28), LetResult=ElseResult30))),
   cl_set_macro_character(#\(?), Lambda, t, _Ignored4).
/*
 e.g. for ?:person
*/
/*
(set-macro-character #\^ 
  (lambda (stream ch)
    (let ((name (read stream t nil t))
          (ob nil))
     (setq ob (ob$name->ob name))
     (if ob
         (list 'quote ob)
         (progn
          (format t "No such ob ^"(set-macro-character #\\^ \n  (lambda (stream ch)\n    (let ((name (read stream t nil t))\n          (ob nil))\n     (setq ob (ob$name->ob name))\n     (if ob\n         (list 'quote ob)\n         (progn\n          (format t \"No such ob ^~A~%\" name)\n          (list 'quote *repl-wont-print*)))))\n  t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:5752 **********************/
:-lisp_compile_to_prolog(pkg_user,['set-macro-character',#\(^),[lambda,[stream,ch],[let,[[name,[read,stream,t,[],t]],[ob,[]]],[setq,ob,['ob$name->ob',name]],[if,ob,[list,[quote,quote],ob],[progn,[format,t,'$STRING'("No such ob ^~A~%"),name],[list,[quote,quote],'*repl-wont-print*']]]]],t])
:- Lambda=closure([ClosureEnvironment|CDR], LetResult, [stream, u_ch],  (get_var(ClosureEnvironment, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Name_Init), LEnv=[bv(sys_name, Name_Init), bv(u_ob, [])|ClosureEnvironment], get_var(LEnv, sys_name, Name_Get), f_u_ob_c36_name_c62_ob(Name_Get, Ob), set_var(LEnv, u_ob, Ob), get_var(LEnv, u_ob, IFTEST), (IFTEST\==[]->get_var(LEnv, u_ob, Ob_Get15), TrueResult=[quote, Ob_Get15], LetResult=TrueResult;get_var(LEnv, sys_name, Name_Get16), cl_format([t, '$ARRAY'([*], claz_base_character, "No such ob ^~A~%"), Name_Get16], Format_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get), ElseResult=[quote, Xx_repl_wont_print_xx_Get], LetResult=ElseResult))),
   cl_set_macro_character(#\(^), Lambda, t, _Ignored4).
/*
(set-macro-character #\!
         (lambda (stream ch)
                 (let ((name (read stream t nil t))
                       (ob nil))
                      (setq ob (ob$name->ob name))
                      (if ob
                          (progn
                           (po ob)
                           (list 'quote *repl-wont-print*))
                          (progn (format t "No such ob ^"(set-macro-character #\\!\n         (lambda (stream ch)\n                 (let ((name (read stream t nil t))\n                       (ob nil))\n                      (setq ob (ob$name->ob name))\n                      (if ob\n                          (progn\n                           (po ob)\n                           (list 'quote *repl-wont-print*))\n                          (progn (format t \"No such ob ^~A~%\" name)\n                         (list 'quote *repl-wont-print*)))))\n  t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6044 **********************/
:-lisp_compile_to_prolog(pkg_user,['set-macro-character',#\(!),[lambda,[stream,ch],[let,[[name,[read,stream,t,[],t]],[ob,[]]],[setq,ob,['ob$name->ob',name]],[if,ob,[progn,[po,ob],[list,[quote,quote],'*repl-wont-print*']],[progn,[format,t,'$STRING'("No such ob ^~A~%"),name],[list,[quote,quote],'*repl-wont-print*']]]]],t])
:- Lambda=closure([ClosureEnvironment|CDR], LetResult, [stream, u_ch],  (get_var(ClosureEnvironment, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Name_Init), LEnv=[bv(sys_name, Name_Init), bv(u_ob, [])|ClosureEnvironment], get_var(LEnv, sys_name, Name_Get), f_u_ob_c36_name_c62_ob(Name_Get, Ob), set_var(LEnv, u_ob, Ob), get_var(LEnv, u_ob, IFTEST), (IFTEST\==[]->get_var(LEnv, u_ob, Ob_Get15), f_u_po(Ob_Get15, Po_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get), TrueResult=[quote, Xx_repl_wont_print_xx_Get], LetResult=TrueResult;get_var(LEnv, sys_name, Name_Get17), cl_format([t, '$ARRAY'([*], claz_base_character, "No such ob ^~A~%"), Name_Get17], Format_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get18), ElseResult=[quote, Xx_repl_wont_print_xx_Get18], LetResult=ElseResult))),
   cl_set_macro_character(#\(!), Lambda, t, _Ignored4).
/*
(defun interrogate (string)
  (format (standard-output) string)
  (let ((response (read (standard-input))))
       (read-line (standard-input))
       (cond ((or (eq? 'y response)
                  (eq? 'yes response)) t)
             ((or (eq? 'n response)
                  (eq? 'no response)) nil)
             (else (format (standard-output)
                           "Please type 'y' or 'n' as a response."(defun interrogate (string)\n  (format (standard-output) string)\n  (let ((response (read (standard-input))))\n       (read-line (standard-input))\n       (cond ((or (eq? 'y response)\n                  (eq? 'yes response)) t)\n             ((or (eq? 'n response)\n                  (eq? 'no response)) nil)\n             (else (format (standard-output)\n                           \"Please type 'y' or 'n' as a response.~%\")\n                   (interrogate string)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6526 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,interrogate,[string],[format,['standard-output'],string],[let,[[response,[read,['standard-input']]]],['read-line',['standard-input']],[cond,[[or,['eq?',[quote,y],response],['eq?',[quote,yes],response]],t],[[or,['eq?',[quote,n],response],['eq?',[quote,no],response]],[]],[else,[format,['standard-output'],'$STRING'("Please type 'y' or 'n' as a response.~%")],[interrogate,string]]]]])
wl:lambda_def(defun, u_interrogate, f_u_interrogate, [string], [[format, [u_standard_output], string], [let, [[u_response, [read, [u_standard_input]]]], [read_line, [u_standard_input]], [cond, [[or, [u_eq_c63, [quote, u_y], u_response], [u_eq_c63, [quote, u_yes], u_response]], t], [[or, [u_eq_c63, [quote, n], u_response], [u_eq_c63, [quote, u_no], u_response]], []], [u_else, [format, [u_standard_output], '$ARRAY'([*], claz_base_character, "Please type 'y' or 'n' as a response.~%")], [u_interrogate, string]]]]]).
wl:arglist_info(u_interrogate, f_u_interrogate, [string], arginfo{all:[string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[string], opt:0, req:[string], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_interrogate).

/*

### Compiled:  `U::INTERROGATE` 
*/
f_u_interrogate(String, ElseResult22) :-
	nop(global_env(Env)),
	Env26=[bv(string, String)|Env],
	f_u_standard_output(Standard_output_Ret),
	cl_format([Standard_output_Ret, string], Format_Ret),
	f_u_standard_input(Read_Param),
	cl_read(Read_Param, Response_Init),
	LEnv=[bv(u_response, Response_Init)|Env26],
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
	->  ElseResult22=t
	;   (   f_u_eq_c63([quote, n], u_response, FORM1_Res16),
		FORM1_Res16\==[],
		IFTEST14=FORM1_Res16
	    ->  true
	    ;   f_u_eq_c63([quote, u_no], u_response, Response29),
		IFTEST14=Response29
	    ),
	    (   IFTEST14\==[]
	    ->  ElseResult22=[]
	    ;   get_var(LEnv, u_else, IFTEST17),
		(   IFTEST17\==[]
		->  f_u_standard_output(Standard_output_Ret35),
		    cl_format(
			      [ Standard_output_Ret35,
				'$ARRAY'([*],
					 claz_base_character,
					 "Please type 'y' or 'n' as a response.~%")
			      ],
			      Format_Ret36),
		    f_u_interrogate(string, TrueResult),
		    ElseResult22=TrueResult
		;   ElseResult22=[]
		)
	    )
	).
:- set_opv(f_u_interrogate, classof, claz_function),
   set_opv(u_interrogate, compile_as, kw_function),
   set_opv(u_interrogate, function, f_u_interrogate),
   _Ignored4=u_interrogate.
/*
:- side_effect(assert_lsp(u_interrogate,
			  wl:lambda_def(defun, u_interrogate, f_u_interrogate, [string], [[format, [u_standard_output], string], [let, [[u_response, [read, [u_standard_input]]]], [read_line, [u_standard_input]], [cond, [[or, [u_eq_c63, [quote, u_y], u_response], [u_eq_c63, [quote, u_yes], u_response]], t], [[or, [u_eq_c63, [quote, n], u_response], [u_eq_c63, [quote, u_no], u_response]], []], [u_else, [format, [u_standard_output], '$ARRAY'([*], claz_base_character, "Please type 'y' or 'n' as a response.~%")], [u_interrogate, string]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_interrogate,
			  wl:arglist_info(u_interrogate, f_u_interrogate, [string], arginfo{all:[string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[string], opt:0, req:[string], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_interrogate,
			  wl:init_args(exact_only, f_u_interrogate))).
*/
/*
(defun arg-value (arg-name init-plist default)
   (let ((found (assq arg-name init-plist)))
     (if (and found (neq? (cadr found) 'none))
               (cadr found)
               (if (eq? default 'required)
                   (error "Required make-instance argument "(defun arg-value (arg-name init-plist default)\n   (let ((found (assq arg-name init-plist)))\n     (if (and found (neq? (cadr found) 'none))\n               (cadr found)\n               (if (eq? default 'required)\n                   (error \"Required make-instance argument ~A not supplied\"\n                          arg-name)\n                   default))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6987 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'arg-value',['arg-name','init-plist',default],[let,[[found,[assq,'arg-name','init-plist']]],[if,[and,found,['neq?',[cadr,found],[quote,none]]],[cadr,found],[if,['eq?',default,[quote,required]],[error,'$STRING'("Required make-instance argument ~A not supplied"),'arg-name'],default]]]])
wl:lambda_def(defun, u_arg_value, f_u_arg_value, [u_arg_name, u_init_plist, u_default], [[let, [[u_found, [ext_assq, u_arg_name, u_init_plist]]], [if, [and, u_found, [u_neq_c63, [cadr, u_found], [quote, u_none]]], [cadr, u_found], [if, [u_eq_c63, u_default, [quote, u_required]], [error, '$ARRAY'([*], claz_base_character, "Required make-instance argument ~A not supplied"), u_arg_name], u_default]]]]).
wl:arglist_info(u_arg_value, f_u_arg_value, [u_arg_name, u_init_plist, u_default], arginfo{all:[u_arg_name, u_init_plist, u_default], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_arg_name, u_init_plist, u_default], opt:0, req:[u_arg_name, u_init_plist, u_default], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_arg_value).

/*

### Compiled:  `U::ARG-VALUE` 
*/
f_u_arg_value(Arg_name, Init_plist, Default, ElseResult25) :-
	nop(global_env(Env)),
	Env28=[bv(u_arg_name, Arg_name), bv(u_init_plist, Init_plist), bv(u_default, Default)|Env],
	f_ext_assq(u_arg_name, u_init_plist, Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env28],
	get_var(LEnv, u_found, IFTEST13),
	(   IFTEST13\==[]
	->  f_u_neq_c63([cadr, u_found], [quote, u_none], TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get17),
	    cl_cadr(Found_Get17, TrueResult24),
	    ElseResult25=TrueResult24
	;   f_u_eq_c63(u_default, [quote, u_required], IFTEST18),
	    (   IFTEST18\==[]
	    ->  get_var(LEnv, u_arg_name, Arg_name_Get),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "Required make-instance argument ~A not supplied"),
			   Arg_name_Get
			 ],
			 TrueResult22),
		ElseResult25=TrueResult22
	    ;   get_var(LEnv, u_default, Default_Get),
		ElseResult25=Default_Get
	    )
	).
:- set_opv(f_u_arg_value, classof, claz_function),
   set_opv(u_arg_value, compile_as, kw_function),
   set_opv(u_arg_value, function, f_u_arg_value),
   _Ignored4=u_arg_value.
/*
:- side_effect(assert_lsp(u_arg_value,
			  wl:lambda_def(defun, u_arg_value, f_u_arg_value, [u_arg_name, u_init_plist, u_default], [[let, [[u_found, [ext_assq, u_arg_name, u_init_plist]]], [if, [and, u_found, [u_neq_c63, [cadr, u_found], [quote, u_none]]], [cadr, u_found], [if, [u_eq_c63, u_default, [quote, u_required]], [error, '$ARRAY'([*], claz_base_character, "Required make-instance argument ~A not supplied"), u_arg_name], u_default]]]]))).
*/
/*
:- side_effect(assert_lsp(u_arg_value,
			  wl:arglist_info(u_arg_value, f_u_arg_value, [u_arg_name, u_init_plist, u_default], arginfo{all:[u_arg_name, u_init_plist, u_default], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_arg_name, u_init_plist, u_default], opt:0, req:[u_arg_name, u_init_plist, u_default], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_arg_value, wl:init_args(exact_only, f_u_arg_value))).
*/
/*
(defun walk-append (proc lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (setq result (append! result (funcall proc (car lst))))
            (setq lst (cdr lst)))
        (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7341 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'walk-append',[proc,lst],[yloop,[initial,[result,[]]],[ywhile,lst],[ydo,[setq,result,['append!',result,[funcall,proc,[car,lst]]]],[setq,lst,[cdr,lst]]],[yresult,result]]])
wl:lambda_def(defun, u_walk_append, f_u_walk_append, [u_proc, u_lst], [[u_yloop, [u_initial, [u_result, []]], [u_ywhile, u_lst], [u_ydo, [setq, u_result, [u_append_c33, u_result, [funcall, u_proc, [car, u_lst]]]], [setq, u_lst, [cdr, u_lst]]], [u_yresult, u_result]]]).
wl:arglist_info(u_walk_append, f_u_walk_append, [u_proc, u_lst], arginfo{all:[u_proc, u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_proc, u_lst], opt:0, req:[u_proc, u_lst], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_walk_append).

/*

### Compiled:  `U::WALK-APPEND` 
*/
f_u_walk_append(Proc, Lst, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_proc, Proc), bv(u_lst, Lst)|Env],
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
   _Ignored4=u_walk_append.
/*
:- side_effect(assert_lsp(u_walk_append,
			  wl:lambda_def(defun, u_walk_append, f_u_walk_append, [u_proc, u_lst], [[u_yloop, [u_initial, [u_result, []]], [u_ywhile, u_lst], [u_ydo, [setq, u_result, [u_append_c33, u_result, [funcall, u_proc, [car, u_lst]]]], [setq, u_lst, [cdr, u_lst]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_walk_append,
			  wl:arglist_info(u_walk_append, f_u_walk_append, [u_proc, u_lst], arginfo{all:[u_proc, u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_proc, u_lst], opt:0, req:[u_proc, u_lst], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_walk_append,
			  wl:init_args(exact_only, f_u_walk_append))).
*/
/*
(defun with-default (val default)
   (if val val default))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7555 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'with-default',[val,default],[if,val,val,default]])
wl:lambda_def(defun, u_with_default, f_u_with_default, [u_val, u_default], [[if, u_val, u_val, u_default]]).
wl:arglist_info(u_with_default, f_u_with_default, [u_val, u_default], arginfo{all:[u_val, u_default], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_val, u_default], opt:0, req:[u_val, u_default], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_with_default).

/*

### Compiled:  `U::WITH-DEFAULT` 
*/
f_u_with_default(Val, Default, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_val, Val), bv(u_default, Default)|Env],
	get_var(Env16, u_val, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env16, u_val, Val_Get10),
	    FnResult=Val_Get10
	;   get_var(Env16, u_default, Default_Get),
	    FnResult=Default_Get
	).
:- set_opv(f_u_with_default, classof, claz_function),
   set_opv(u_with_default, compile_as, kw_function),
   set_opv(u_with_default, function, f_u_with_default),
   _Ignored4=u_with_default.
/*
:- side_effect(assert_lsp(u_with_default,
			  wl:lambda_def(defun, u_with_default, f_u_with_default, [u_val, u_default], [[if, u_val, u_val, u_default]]))).
*/
/*
:- side_effect(assert_lsp(u_with_default,
			  wl:arglist_info(u_with_default, f_u_with_default, [u_val, u_default], arginfo{all:[u_val, u_default], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_val, u_default], opt:0, req:[u_val, u_default], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_with_default,
			  wl:init_args(exact_only, f_u_with_default))).
*/
/*
(defun random-integer (from to)
  (cond ((= to from) to)
        ((< to from) (random-integer to from))
        (else (+ from (random (1+ (- to from)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7615 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'random-integer',[from,to],[cond,[[=,to,from],to],[[<,to,from],['random-integer',to,from]],[else,[+,from,[random,['1+',[-,to,from]]]]]]])
wl:lambda_def(defun, u_random_integer, f_u_random_integer, [u_from, u_to], [[cond, [[=, u_to, u_from], u_to], [[<, u_to, u_from], [u_random_integer, u_to, u_from]], [u_else, [+, u_from, [random, ['1+', [-, u_to, u_from]]]]]]]).
wl:arglist_info(u_random_integer, f_u_random_integer, [u_from, u_to], arginfo{all:[u_from, u_to], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from, u_to], opt:0, req:[u_from, u_to], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_random_integer).

/*

### Compiled:  `U::RANDOM-INTEGER` 
*/
f_u_random_integer(From, To, ElseResult31) :-
	nop(global_env(Env)),
	Env36=[bv(u_from, From), bv(u_to, To)|Env],
	get_var(Env36, u_from, From_Get),
	get_var(Env36, u_to, To_Get),
	(   To_Get=:=From_Get
	->  get_var(Env36, u_to, To_Get13),
	    ElseResult31=To_Get13
	;   get_var(Env36, u_from, From_Get16),
	    get_var(Env36, u_to, To_Get15),
	    (   To_Get15<From_Get16
	    ->  get_var(Env36, u_from, From_Get21),
		get_var(Env36, u_to, To_Get20),
		f_u_random_integer(To_Get20, From_Get21, TrueResult30),
		ElseResult31=TrueResult30
	    ;   get_var(Env36, u_else, IFTEST22),
		(   IFTEST22\==[]
		->  get_var(Env36, u_from, From_Get25),
		    get_var(Env36, u_to, To_Get26),
		    -(To_Get26, From_Get25, _395736722),
		    '1+'(_395736722, Random_Param),
		    cl_random(Random_Param, Random_Ret),
		    +(From_Get25, Random_Ret, TrueResult),
		    ElseResult31=TrueResult
		;   ElseResult31=[]
		)
	    )
	).
:- set_opv(f_u_random_integer, classof, claz_function),
   set_opv(u_random_integer, compile_as, kw_function),
   set_opv(u_random_integer, function, f_u_random_integer),
   _Ignored4=u_random_integer.
/*
:- side_effect(assert_lsp(u_random_integer,
			  wl:lambda_def(defun, u_random_integer, f_u_random_integer, [u_from, u_to], [[cond, [[=, u_to, u_from], u_to], [[<, u_to, u_from], [u_random_integer, u_to, u_from]], [u_else, [+, u_from, [random, ['1+', [-, u_to, u_from]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_random_integer,
			  wl:arglist_info(u_random_integer, f_u_random_integer, [u_from, u_to], arginfo{all:[u_from, u_to], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from, u_to], opt:0, req:[u_from, u_to], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_random_integer,
			  wl:init_args(exact_only, f_u_random_integer))).
*/
/*
(defun random-element (x)
  (nth-elem x (random-integer 0 (-1+ (length x)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7772 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'random-element',[x],['nth-elem',x,['random-integer',0,['-1+',[length,x]]]]])
wl:lambda_def(defun, u_random_element, f_u_random_element, [u_x], [[u_nth_elem, u_x, [u_random_integer, 0, ['-1+', [length, u_x]]]]]).
wl:arglist_info(u_random_element, f_u_random_element, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_random_element).

/*

### Compiled:  `U::RANDOM-ELEMENT` 
*/
f_u_random_element(X, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_x, X)|Env],
	f_u_nth_elem(u_x,
		     [u_random_integer, 0, ['-1+', [length, u_x]]],
		     Nth_elem_Ret),
	Nth_elem_Ret=FnResult.
:- set_opv(f_u_random_element, classof, claz_function),
   set_opv(u_random_element, compile_as, kw_function),
   set_opv(u_random_element, function, f_u_random_element),
   _Ignored4=u_random_element.
/*
:- side_effect(assert_lsp(u_random_element,
			  wl:lambda_def(defun, u_random_element, f_u_random_element, [u_x], [[u_nth_elem, u_x, [u_random_integer, 0, ['-1+', [length, u_x]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_random_element,
			  wl:arglist_info(u_random_element, f_u_random_element, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_random_element,
			  wl:init_args(exact_only, f_u_random_element))).
*/
/*
(defun randomize (x)
  (yloop (initial (result nil)
                 (elem nil))
        (ywhile x)
        (ydo (setq elem (random-element x))
            (setq x (delq elem x))
            (setq result (cons elem result)))
        (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7851 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,randomize,[x],[yloop,[initial,[result,[]],[elem,[]]],[ywhile,x],[ydo,[setq,elem,['random-element',x]],[setq,x,[delq,elem,x]],[setq,result,[cons,elem,result]]],[yresult,result]]])
wl:lambda_def(defun, u_randomize, f_u_randomize, [u_x], [[u_yloop, [u_initial, [u_result, []], [u_elem, []]], [u_ywhile, u_x], [u_ydo, [setq, u_elem, [u_random_element, u_x]], [setq, u_x, [u_delq, u_elem, u_x]], [setq, u_result, [cons, u_elem, u_result]]], [u_yresult, u_result]]]).
wl:arglist_info(u_randomize, f_u_randomize, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_randomize).

/*

### Compiled:  `U::RANDOMIZE` 
*/
f_u_randomize(X, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_x, X)|Env],
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
   _Ignored4=u_randomize.
/*
:- side_effect(assert_lsp(u_randomize,
			  wl:lambda_def(defun, u_randomize, f_u_randomize, [u_x], [[u_yloop, [u_initial, [u_result, []], [u_elem, []]], [u_ywhile, u_x], [u_ydo, [setq, u_elem, [u_random_element, u_x]], [setq, u_x, [u_delq, u_elem, u_x]], [setq, u_result, [cons, u_elem, u_result]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_randomize,
			  wl:arglist_info(u_randomize, f_u_randomize, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_randomize, wl:init_args(exact_only, f_u_randomize))).
*/
/*
(defun force-flonum (x)
  (if (flonum? x) x (fixnum->flonum x)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:8104 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'force-flonum',[x],[if,['flonum?',x],x,['fixnum->flonum',x]]])
wl:lambda_def(defun, u_force_flonum, f_u_force_flonum, [u_x], [[if, [u_flonum_c63, u_x], u_x, [u_fixnum_c62_flonum, u_x]]]).
wl:arglist_info(u_force_flonum, f_u_force_flonum, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_force_flonum).

/*

### Compiled:  `U::FORCE-FLONUM` 
*/
f_u_force_flonum(X, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_x, X)|Env],
	f_u_flonum_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_x, X_Get),
	    FnResult=X_Get
	;   f_u_fixnum_c62_flonum(u_x, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_force_flonum, classof, claz_function),
   set_opv(u_force_flonum, compile_as, kw_function),
   set_opv(u_force_flonum, function, f_u_force_flonum),
   _Ignored4=u_force_flonum.
/*
:- side_effect(assert_lsp(u_force_flonum,
			  wl:lambda_def(defun, u_force_flonum, f_u_force_flonum, [u_x], [[if, [u_flonum_c63, u_x], u_x, [u_fixnum_c62_flonum, u_x]]]))).
*/
/*
:- side_effect(assert_lsp(u_force_flonum,
			  wl:arglist_info(u_force_flonum, f_u_force_flonum, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_force_flonum,
			  wl:init_args(exact_only, f_u_force_flonum))).
*/
/*
(defun random-real (from to)
  (setq *large-integer* (random-integer 4 20))
  (cond ((= to from) to)
        ((< to from) (random-real to from))
        (else
         (+ from
            (* (- to from)
               (/ (force-flonum (random-integer 0 *large-integer*))
                  (force-flonum *large-integer*)))))))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:8170 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'random-real',[from,to],[setq,'*large-integer*',['random-integer',4,20]],[cond,[[=,to,from],to],[[<,to,from],['random-real',to,from]],[else,[+,from,[*,[-,to,from],[/,['force-flonum',['random-integer',0,'*large-integer*']],['force-flonum','*large-integer*']]]]]]])
wl:lambda_def(defun, u_random_real, f_u_random_real, [u_from, u_to], [[setq, u_xx_large_integer_xx, [u_random_integer, 4, 20]], [cond, [[=, u_to, u_from], u_to], [[<, u_to, u_from], [u_random_real, u_to, u_from]], [u_else, [+, u_from, [*, [-, u_to, u_from], [/, [u_force_flonum, [u_random_integer, 0, u_xx_large_integer_xx]], [u_force_flonum, u_xx_large_integer_xx]]]]]]]).
wl:arglist_info(u_random_real, f_u_random_real, [u_from, u_to], arginfo{all:[u_from, u_to], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from, u_to], opt:0, req:[u_from, u_to], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_random_real).

/*

### Compiled:  `U::RANDOM-REAL` 
*/
f_u_random_real(From, To, ElseResult34) :-
	nop(global_env(Env)),
	AEnv=[bv(u_from, From), bv(u_to, To)|Env],
	f_u_random_integer(4, 20, Xx_large_integer_xx),
	set_var(AEnv, u_xx_large_integer_xx, Xx_large_integer_xx),
	get_var(AEnv, u_from, From_Get),
	get_var(AEnv, u_to, To_Get),
	(   To_Get=:=From_Get
	->  get_var(AEnv, u_to, To_Get14),
	    ElseResult34=To_Get14
	;   get_var(AEnv, u_from, From_Get17),
	    get_var(AEnv, u_to, To_Get16),
	    (   To_Get16<From_Get17
	    ->  get_var(AEnv, u_from, From_Get22),
		get_var(AEnv, u_to, To_Get21),
		f_u_random_real(To_Get21, From_Get22, TrueResult33),
		ElseResult34=TrueResult33
	    ;   get_var(AEnv, u_else, IFTEST23),
		(   IFTEST23\==[]
		->  get_var(AEnv, u_from, From_Get26),
		    get_var(AEnv, u_to, To_Get27),
		    -(To_Get27, From_Get26, _398751234),
		    get_var(AEnv,
			    u_xx_large_integer_xx,
			    Xx_large_integer_xx_Get),
		    f_u_random_integer(0,
				       Xx_large_integer_xx_Get,
				       Force_flonum_Param),
		    f_u_force_flonum(Force_flonum_Param, Force_flonum_Ret),
		    get_var(AEnv,
			    u_xx_large_integer_xx,
			    Xx_large_integer_xx_Get30),
		    f_u_force_flonum(Xx_large_integer_xx_Get30,
				     Force_flonum_Ret44),
		    /(Force_flonum_Ret, Force_flonum_Ret44, _398754054),
		    *(_398751234, _398754054, _398750818),
		    +(From_Get26, _398750818, TrueResult),
		    ElseResult34=TrueResult
		;   ElseResult34=[]
		)
	    )
	).
:- set_opv(f_u_random_real, classof, claz_function),
   set_opv(u_random_real, compile_as, kw_function),
   set_opv(u_random_real, function, f_u_random_real),
   _Ignored4=u_random_real.
/*
:- side_effect(assert_lsp(u_random_real,
			  wl:lambda_def(defun, u_random_real, f_u_random_real, [u_from, u_to], [[setq, u_xx_large_integer_xx, [u_random_integer, 4, 20]], [cond, [[=, u_to, u_from], u_to], [[<, u_to, u_from], [u_random_real, u_to, u_from]], [u_else, [+, u_from, [*, [-, u_to, u_from], [/, [u_force_flonum, [u_random_integer, 0, u_xx_large_integer_xx]], [u_force_flonum, u_xx_large_integer_xx]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_random_real,
			  wl:arglist_info(u_random_real, f_u_random_real, [u_from, u_to], arginfo{all:[u_from, u_to], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from, u_to], opt:0, req:[u_from, u_to], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_random_real,
			  wl:init_args(exact_only, f_u_random_real))).
*/
/*
 End of file.
*/


%; Total compilation time: 6.43 seconds


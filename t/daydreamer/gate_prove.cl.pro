#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_prove" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:15:15 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
 (load "gate_read_pr")
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
 This file contains the OB theorem prover
*/
/*
*/
/*
  2/23/87: First version written
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *prules* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:363 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*prules*',[]])
:- set_var(AEnv, setq, u_xx_prules_xx, []).
/*
(defun ob$add-prule (prule)
  (setq *prules* (cons prule *prules*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:385 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$add-prule',[prule],[setq,'*prules*',[cons,prule,'*prules*']]])
wl:lambda_def(defun, u_ob_c36_add_prule, f_u_ob_c36_add_prule, [u_prule], [[setq, u_xx_prules_xx, [cons, u_prule, u_xx_prules_xx]]]).
wl:arglist_info(u_ob_c36_add_prule, f_u_ob_c36_add_prule, [u_prule], arginfo{all:[u_prule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prule], opt:0, req:[u_prule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_add_prule).

/*

### Compiled:  `U::OB$ADD-PRULE` 
*/
f_u_ob_c36_add_prule(Prule, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_prule, Prule)|Env],
	get_var(AEnv, u_prule, Prule_Get),
	get_var(AEnv, u_xx_prules_xx, Xx_prules_xx_Get),
	Xx_prules_xx=[Prule_Get|Xx_prules_xx_Get],
	set_var(AEnv, u_xx_prules_xx, Xx_prules_xx),
	Xx_prules_xx=FnResult.
:- set_opv(f_u_ob_c36_add_prule, classof, claz_function),
   set_opv(u_ob_c36_add_prule, compile_as, kw_function),
   set_opv(u_ob_c36_add_prule, function, f_u_ob_c36_add_prule),
   _Ignored4=u_ob_c36_add_prule.
/*
:- side_effect(assert_lsp(u_ob_c36_add_prule,
			  wl:lambda_def(defun, u_ob_c36_add_prule, f_u_ob_c36_add_prule, [u_prule], [[setq, u_xx_prules_xx, [cons, u_prule, u_xx_prules_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_prule,
			  wl:arglist_info(u_ob_c36_add_prule, f_u_ob_c36_add_prule, [u_prule], arginfo{all:[u_prule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prule], opt:0, req:[u_prule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_prule,
			  wl:init_args(exact_only, f_u_ob_c36_add_prule))).
*/
/*
(defun ob$remove-prule (prule)
  (setq *prules* (delq! prule *prules*)))

;*******************************************************************************
;
; ob$prove:
;
; pattern - concept, possibly containing variables (i.e., a query), to prove
; bd - binding list with respect to which the proof is to be performed
; max-number - the maximum number of solutions that are to be generated
;
; ob$prove1:
;
; pfacts - context containing the 'facts' which may be used in the proof
; prules - list of obs which are the 'prules' which may be used in the proof
; ignore-slots - list of slots to ignore
;
; Sample rules demonstrating the use of ROR, RAND, and RNOT:
;
; (ob$fcreate '(PRULE subgoal (ROR obj (PTRANS actor ?Person to ?Location)
;                                  obj (LIVES-IN actor ?Person loc ?Location))
;                     goal (PROX actor ?Person loc ?Location)))
;
; A solution is an augmented binding list. Since web-prove can generate
; several solutions, the result is a list of augmented binding lists. Thus,
; ob$prove returns either:
;
; 1) NIL if con cannot be proved
; 2) list of augmented binding lists if con can be proved
;
; Still to do:
; Add negation
;
;*******************************************************************************

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$remove-prule',[prule],[setq,'*prules*',['delq!',prule,'*prules*']]])
wl:lambda_def(defun, u_ob_c36_remove_prule, f_u_ob_c36_remove_prule, [u_prule], [[setq, u_xx_prules_xx, [u_delq_c33, u_prule, u_xx_prules_xx]]]).
wl:arglist_info(u_ob_c36_remove_prule, f_u_ob_c36_remove_prule, [u_prule], arginfo{all:[u_prule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prule], opt:0, req:[u_prule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_remove_prule).

/*

### Compiled:  `U::OB$REMOVE-PRULE` 
*/
f_u_ob_c36_remove_prule(Prule, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_prule, Prule)|Env],
	f_u_delq_c33(u_prule, u_xx_prules_xx, Xx_prules_xx),
	set_var(AEnv, u_xx_prules_xx, Xx_prules_xx),
	Xx_prules_xx=FnResult.
:- set_opv(f_u_ob_c36_remove_prule, classof, claz_function),
   set_opv(u_ob_c36_remove_prule, compile_as, kw_function),
   set_opv(u_ob_c36_remove_prule, function, f_u_ob_c36_remove_prule),
   _Ignored4=u_ob_c36_remove_prule.
/*
:- side_effect(assert_lsp(u_ob_c36_remove_prule,
			  wl:lambda_def(defun, u_ob_c36_remove_prule, f_u_ob_c36_remove_prule, [u_prule], [[setq, u_xx_prules_xx, [u_delq_c33, u_prule, u_xx_prules_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_prule,
			  wl:arglist_info(u_ob_c36_remove_prule, f_u_ob_c36_remove_prule, [u_prule], arginfo{all:[u_prule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prule], opt:0, req:[u_prule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_prule,
			  wl:init_args(exact_only, f_u_ob_c36_remove_prule))).
*/
/*
*******************************************************************************
*/
/*
*/
/*
 ob$prove:
*/
/*
*/
/*
 pattern - concept, possibly containing variables (i.e., a query), to prove
*/
/*
 bd - binding list with respect to which the proof is to be performed
*/
/*
 max-number - the maximum number of solutions that are to be generated
*/
/*
*/
/*
 ob$prove1:
*/
/*
*/
/*
 pfacts - context containing the 'facts' which may be used in the proof
*/
/*
 prules - list of obs which are the 'prules' which may be used in the proof
*/
/*
 ignore-slots - list of slots to ignore
*/
/*
*/
/*
 Sample rules demonstrating the use of ROR, RAND, and RNOT:
*/
/*
*/
/*
 (ob$fcreate '(PRULE subgoal (ROR obj (PTRANS actor ?Person to ?Location)
*/
/*
                                  obj (LIVES-IN actor ?Person loc ?Location))
*/
/*
                     goal (PROX actor ?Person loc ?Location)))
*/
/*
*/
/*
 A solution is an augmented binding list. Since web-prove can generate
*/
/*
 several solutions, the result is a list of augmented binding lists. Thus,
*/
/*
 ob$prove returns either:
*/
/*
*/
/*
 1) NIL if con cannot be proved
*/
/*
 2) list of augmented binding lists if con can be proved
*/
/*
*/
/*
 Still to do:
*/
/*
 Add negation
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *proof-failures* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1723 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*proof-failures*',[]])
:- set_var(AEnv, setq, u_xx_proof_failures_xx, []).
/*
(defun ob$prove (pattern bd max-number)
  (ob$prove1 pattern bd max-number *prules* *pfacts* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1752 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$prove',[pattern,bd,'max-number'],['ob$prove1',pattern,bd,'max-number','*prules*','*pfacts*',[]]])
wl:lambda_def(defun, u_ob_c36_prove, f_u_ob_c36_prove, [u_pattern, u_bd, u_max_number], [[u_ob_c36_prove1, u_pattern, u_bd, u_max_number, u_xx_prules_xx, u_xx_pfacts_xx, []]]).
wl:arglist_info(u_ob_c36_prove, f_u_ob_c36_prove, [u_pattern, u_bd, u_max_number], arginfo{all:[u_pattern, u_bd, u_max_number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number], opt:0, req:[u_pattern, u_bd, u_max_number], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_prove).

/*

### Compiled:  `U::OB$PROVE` 
*/
f_u_ob_c36_prove(Pattern, Bd, Max_number, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_pattern, Pattern), bv(u_bd, Bd), bv(u_max_number, Max_number)|Env],
	get_var(Env14, u_bd, Bd_Get),
	get_var(Env14, u_max_number, Max_number_Get),
	get_var(Env14, u_pattern, Pattern_Get),
	get_var(Env14, u_xx_pfacts_xx, Xx_pfacts_xx_Get),
	get_var(Env14, u_xx_prules_xx, Xx_prules_xx_Get),
	f_u_ob_c36_prove1(Pattern_Get,
			  Bd_Get,
			  Max_number_Get,
			  Xx_prules_xx_Get,
			  Xx_pfacts_xx_Get,
			  [],
			  C36_prove1_Ret),
	C36_prove1_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove, classof, claz_function),
   set_opv(u_ob_c36_prove, compile_as, kw_function),
   set_opv(u_ob_c36_prove, function, f_u_ob_c36_prove),
   _Ignored4=u_ob_c36_prove.
/*
:- side_effect(assert_lsp(u_ob_c36_prove,
			  wl:lambda_def(defun, u_ob_c36_prove, f_u_ob_c36_prove, [u_pattern, u_bd, u_max_number], [[u_ob_c36_prove1, u_pattern, u_bd, u_max_number, u_xx_prules_xx, u_xx_pfacts_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove,
			  wl:arglist_info(u_ob_c36_prove, f_u_ob_c36_prove, [u_pattern, u_bd, u_max_number], arginfo{all:[u_pattern, u_bd, u_max_number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number], opt:0, req:[u_pattern, u_bd, u_max_number], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove,
			  wl:init_args(exact_only, f_u_ob_c36_prove))).
*/
/*
(defun ob$prove1 (pattern bd max-number prules pfacts ignore-slots)
  (setq *proof-failures* nil)
  (ob$prove2 pattern bd max-number prules pfacts ignore-slots))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1852 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$prove1',[pattern,bd,'max-number',prules,pfacts,'ignore-slots'],[setq,'*proof-failures*',[]],['ob$prove2',pattern,bd,'max-number',prules,pfacts,'ignore-slots']])
wl:lambda_def(defun, u_ob_c36_prove1, f_u_ob_c36_prove1, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[setq, u_xx_proof_failures_xx, []], [u_ob_c36_prove2, u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]).
wl:arglist_info(u_ob_c36_prove1, f_u_ob_c36_prove1, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_prove1).

/*

### Compiled:  `U::OB$PROVE1` 
*/
f_u_ob_c36_prove1(Pattern, Bd, Max_number, Prules, Pfacts, Ignore_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_pattern, Pattern), bv(u_bd, Bd), bv(u_max_number, Max_number), bv(u_prules, Prules), bv(u_pfacts, Pfacts), bv(u_ignore_slots, Ignore_slots)|Env],
	set_var(AEnv, setq, u_xx_proof_failures_xx, []),
	get_var(AEnv, u_bd, Bd_Get),
	get_var(AEnv, u_ignore_slots, Ignore_slots_Get),
	get_var(AEnv, u_max_number, Max_number_Get),
	get_var(AEnv, u_pattern, Pattern_Get),
	get_var(AEnv, u_pfacts, Pfacts_Get),
	get_var(AEnv, u_prules, Prules_Get),
	f_u_ob_c36_prove2(Pattern_Get,
			  Bd_Get,
			  Max_number_Get,
			  Prules_Get,
			  Pfacts_Get,
			  Ignore_slots_Get,
			  C36_prove2_Ret),
	C36_prove2_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove1, classof, claz_function),
   set_opv(u_ob_c36_prove1, compile_as, kw_function),
   set_opv(u_ob_c36_prove1, function, f_u_ob_c36_prove1),
   _Ignored4=u_ob_c36_prove1.
/*
:- side_effect(assert_lsp(u_ob_c36_prove1,
			  wl:lambda_def(defun, u_ob_c36_prove1, f_u_ob_c36_prove1, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[setq, u_xx_proof_failures_xx, []], [u_ob_c36_prove2, u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove1,
			  wl:arglist_info(u_ob_c36_prove1, f_u_ob_c36_prove1, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove1,
			  wl:init_args(exact_only, f_u_ob_c36_prove1))).
*/
/*
(defun ob$prove2 (pattern bd max-number prules pfacts ignore-slots)
  (cond
   ((ty$instance? pattern 'rand)
    (ob$prove-all (ob$gets pattern 'obj)
                  bd max-number prules pfacts ignore-slots))
   ((ty$instance? pattern 'ror)
    (ob$prove-any (ob$gets pattern 'obj)
                  bd max-number prules pfacts ignore-slots))
   ((ty$instance? pattern 'rnot)
    (if (ob$prove2 (ob$get pattern 'obj)
                   bd max-number
                   prules pfacts ignore-slots)
        nil
        bd))
   (else
    (yloop
     (initial (result
               (map 'list  (lambda (elem) (cons nil (cdr elem)))
                    (cx$retrieve-bd pfacts pattern bd)))
              (new-bd nil)
              (result1 nil))
     (yfor prule in prules)
     (ywhile (< (length result) max-number))
     (ydo
      (if (setq new-bd (ob$unify1 (ob$get prule 'goal) pattern bd
                                  ignore-slots))
          (progn
           (if (setq result1
                (ob$prove2 (ob$instantiate (ob$get prule 'subgoal) new-bd)
                           bd max-number prules pfacts ignore-slots))
               (setq result (append! result
                                     (map 'list 
                                      (lambda (elem)
                                       (cons t ;(cons prule (car elem))
                                             (cdr elem)))
                                      result1)))))))
     (yresult
      (if (and (null? result) (not (memq? pattern *proof-failures*)))
          (setq *proof-failures* (cons (list pattern bd) *proof-failures*)))
      result)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:2015 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$prove2',[pattern,bd,'max-number',prules,pfacts,'ignore-slots'],[cond,[['ty$instance?',pattern,[quote,rand]],['ob$prove-all',['ob$gets',pattern,[quote,obj]],bd,'max-number',prules,pfacts,'ignore-slots']],[['ty$instance?',pattern,[quote,ror]],['ob$prove-any',['ob$gets',pattern,[quote,obj]],bd,'max-number',prules,pfacts,'ignore-slots']],[['ty$instance?',pattern,[quote,rnot]],[if,['ob$prove2',['ob$get',pattern,[quote,obj]],bd,'max-number',prules,pfacts,'ignore-slots'],[],bd]],[else,[yloop,[initial,[result,[map,[quote,list],[lambda,[elem],[cons,[],[cdr,elem]]],['cx$retrieve-bd',pfacts,pattern,bd]]],['new-bd',[]],[result1,[]]],[yfor,prule,in,prules],[ywhile,[<,[length,result],'max-number']],[ydo,[if,[setq,'new-bd',['ob$unify1',['ob$get',prule,[quote,goal]],pattern,bd,'ignore-slots']],[progn,[if,[setq,result1,['ob$prove2',['ob$instantiate',['ob$get',prule,[quote,subgoal]],'new-bd'],bd,'max-number',prules,pfacts,'ignore-slots']],[setq,result,['append!',result,[map,[quote,list],[lambda,[elem],[cons,t,[cdr,elem]]],result1]]]]]]],[yresult,[if,[and,['null?',result],[not,['memq?',pattern,'*proof-failures*']]],[setq,'*proof-failures*',[cons,[list,pattern,bd],'*proof-failures*']]],result]]]]])
wl:lambda_def(defun, u_ob_c36_prove2, f_u_ob_c36_prove2, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[cond, [[u_ty_c36_instance_c63, u_pattern, [quote, u_rand]], [u_ob_c36_prove_all, [u_ob_c36_gets, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [[u_ty_c36_instance_c63, u_pattern, [quote, u_ror]], [u_ob_c36_prove_any, [u_ob_c36_gets, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [[u_ty_c36_instance_c63, u_pattern, [quote, u_rnot]], [if, [u_ob_c36_prove2, [u_ob_c36_get, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [], u_bd]], [u_else, [u_yloop, [u_initial, [u_result, [map, [quote, list], [lambda, [u_elem], [cons, [], [cdr, u_elem]]], [u_cx_c36_retrieve_bd, u_pfacts, u_pattern, u_bd]]], [u_new_bd, []], [u_result1, []]], [u_yfor, u_prule, u_in, u_prules], [u_ywhile, [<, [length, u_result], u_max_number]], [u_ydo, [if, [setq, u_new_bd, [u_ob_c36_unify1, [u_ob_c36_get, u_prule, [quote, u_goal]], u_pattern, u_bd, u_ignore_slots]], [progn, [if, [setq, u_result1, [u_ob_c36_prove2, [u_ob_c36_instantiate, [u_ob_c36_get, u_prule, [quote, u_subgoal]], u_new_bd], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [setq, u_result, [u_append_c33, u_result, [map, [quote, list], [lambda, [u_elem], [cons, t, [cdr, u_elem]]], u_result1]]]]]]], [u_yresult, [if, [and, [u_null_c63, u_result], [not, [u_memq_c63, u_pattern, u_xx_proof_failures_xx]]], [setq, u_xx_proof_failures_xx, [cons, [list, u_pattern, u_bd], u_xx_proof_failures_xx]]], u_result]]]]]).
wl:arglist_info(u_ob_c36_prove2, f_u_ob_c36_prove2, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_prove2).

/*

### Compiled:  `U::OB$PROVE2` 
*/
f_u_ob_c36_prove2(Pattern, Bd, Max_number, Prules, Pfacts, Ignore_slots, TrueResult43) :-
	nop(global_env(Env)),
	Env51=[bv(u_pattern, Pattern), bv(u_bd, Bd), bv(u_max_number, Max_number), bv(u_prules, Prules), bv(u_pfacts, Pfacts), bv(u_ignore_slots, Ignore_slots)|Env],
	get_var(Env51, u_pattern, Pattern_Get),
	f_u_ty_c36_instance_c63(Pattern_Get, u_rand, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env51, u_pattern, Pattern_Get10),
	    f_u_ob_c36_gets(Pattern_Get10, u_obj, Obj),
	    get_var(Env51, u_bd, Bd_Get),
	    get_var(Env51, u_ignore_slots, Ignore_slots_Get),
	    get_var(Env51, u_max_number, Max_number_Get),
	    get_var(Env51, u_pfacts, Pfacts_Get),
	    get_var(Env51, u_prules, Prules_Get),
	    f_u_ob_c36_prove_all(Obj,
				 Bd_Get,
				 Max_number_Get,
				 Prules_Get,
				 Pfacts_Get,
				 Ignore_slots_Get,
				 TrueResult47),
	    TrueResult43=TrueResult47
	;   get_var(Env51, u_pattern, Pattern_Get18),
	    f_u_ty_c36_instance_c63(Pattern_Get18, u_ror, IFTEST16),
	    (   IFTEST16\==[]
	    ->  get_var(Env51, u_pattern, Pattern_Get19),
		f_u_ob_c36_gets(Pattern_Get19, u_obj, Obj59),
		get_var(Env51, u_bd, Bd_Get20),
		get_var(Env51, u_ignore_slots, Ignore_slots_Get24),
		get_var(Env51, u_max_number, Max_number_Get21),
		get_var(Env51, u_pfacts, Pfacts_Get23),
		get_var(Env51, u_prules, Prules_Get22),
		f_u_ob_c36_prove_any(Obj59,
				     Bd_Get20,
				     Max_number_Get21,
				     Prules_Get22,
				     Pfacts_Get23,
				     Ignore_slots_Get24,
				     TrueResult45),
		TrueResult43=TrueResult45
	    ;   get_var(Env51, u_pattern, Pattern_Get27),
		f_u_ty_c36_instance_c63(Pattern_Get27, u_rnot, IFTEST25),
		(   IFTEST25\==[]
		->  get_var(Env51, u_pattern, Pattern_Get30),
		    f_u_ob_c36_get(Pattern_Get30, u_obj, Obj60),
		    get_var(Env51, u_bd, Bd_Get31),
		    get_var(Env51, u_ignore_slots, Ignore_slots_Get35),
		    get_var(Env51, u_max_number, Max_number_Get32),
		    get_var(Env51, u_pfacts, Pfacts_Get34),
		    get_var(Env51, u_prules, Prules_Get33),
		    f_u_ob_c36_prove2(Obj60,
				      Bd_Get31,
				      Max_number_Get32,
				      Prules_Get33,
				      Pfacts_Get34,
				      Ignore_slots_Get35,
				      IFTEST28),
		    (   IFTEST28\==[]
		    ->  TrueResult43=[]
		    ;   get_var(Env51, u_bd, Bd_Get36),
			TrueResult43=Bd_Get36
		    )
		;   get_var(Env51, u_else, IFTEST38),
		    (   IFTEST38\==[]
		    ->  f_u_yloop(
				  [ 
				    [ u_initial,
				      
				      [ u_result,
					
					[ map,
					  [quote, list],
					  
					  [ lambda,
					    [u_elem],
					    [cons, [], [cdr, u_elem]]
					  ],
					  
					  [ u_cx_c36_retrieve_bd,
					    u_pfacts,
					    u_pattern,
					    u_bd
					  ]
					]
				      ],
				      [u_new_bd, []],
				      [u_result1, []]
				    ],
				    [u_yfor, u_prule, u_in, u_prules],
				    
				    [ u_ywhile,
				      [<, [length, u_result], u_max_number]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ setq,
					  u_new_bd,
					  
					  [ u_ob_c36_unify1,
					    
					    [ u_ob_c36_get,
					      u_prule,
					      [quote, u_goal]
					    ],
					    u_pattern,
					    u_bd,
					    u_ignore_slots
					  ]
					],
					
					[ progn,
					  
					  [ if,
					    
					    [ setq,
					      u_result1,
					      
					      [ u_ob_c36_prove2,
						
						[ u_ob_c36_instantiate,
						  
						  [ u_ob_c36_get,
						    u_prule,
						    [quote, u_subgoal]
						  ],
						  u_new_bd
						],
						u_bd,
						u_max_number,
						u_prules,
						u_pfacts,
						u_ignore_slots
					      ]
					    ],
					    
					    [ setq,
					      u_result,
					      
					      [ u_append_c33,
						u_result,
						
						[ map,
						  [quote, list],
						  
						  [ lambda,
						    [u_elem],
						    [cons, t, [cdr, u_elem]]
						  ],
						  u_result1
						]
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_yresult,
				      
				      [ if,
					
					[ and,
					  [u_null_c63, u_result],
					  
					  [ not,
					    
					    [ u_memq_c63,
					      u_pattern,
					      u_xx_proof_failures_xx
					    ]
					  ]
					],
					
					[ setq,
					  u_xx_proof_failures_xx,
					  
					  [ cons,
					    [list, u_pattern, u_bd],
					    u_xx_proof_failures_xx
					  ]
					]
				      ],
				      u_result
				    ]
				  ],
				  TrueResult),
			TrueResult43=TrueResult
		    ;   TrueResult43=[]
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_prove2, classof, claz_function),
   set_opv(u_ob_c36_prove2, compile_as, kw_function),
   set_opv(u_ob_c36_prove2, function, f_u_ob_c36_prove2),
   _Ignored4=u_ob_c36_prove2.
/*
:- side_effect(assert_lsp(u_ob_c36_prove2,
			  wl:lambda_def(defun, u_ob_c36_prove2, f_u_ob_c36_prove2, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[cond, [[u_ty_c36_instance_c63, u_pattern, [quote, u_rand]], [u_ob_c36_prove_all, [u_ob_c36_gets, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [[u_ty_c36_instance_c63, u_pattern, [quote, u_ror]], [u_ob_c36_prove_any, [u_ob_c36_gets, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [[u_ty_c36_instance_c63, u_pattern, [quote, u_rnot]], [if, [u_ob_c36_prove2, [u_ob_c36_get, u_pattern, [quote, u_obj]], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [], u_bd]], [u_else, [u_yloop, [u_initial, [u_result, [map, [quote, list], [lambda, [u_elem], [cons, [], [cdr, u_elem]]], [u_cx_c36_retrieve_bd, u_pfacts, u_pattern, u_bd]]], [u_new_bd, []], [u_result1, []]], [u_yfor, u_prule, u_in, u_prules], [u_ywhile, [<, [length, u_result], u_max_number]], [u_ydo, [if, [setq, u_new_bd, [u_ob_c36_unify1, [u_ob_c36_get, u_prule, [quote, u_goal]], u_pattern, u_bd, u_ignore_slots]], [progn, [if, [setq, u_result1, [u_ob_c36_prove2, [u_ob_c36_instantiate, [u_ob_c36_get, u_prule, [quote, u_subgoal]], u_new_bd], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]], [setq, u_result, [u_append_c33, u_result, [map, [quote, list], [lambda, [u_elem], [cons, t, [cdr, u_elem]]], u_result1]]]]]]], [u_yresult, [if, [and, [u_null_c63, u_result], [not, [u_memq_c63, u_pattern, u_xx_proof_failures_xx]]], [setq, u_xx_proof_failures_xx, [cons, [list, u_pattern, u_bd], u_xx_proof_failures_xx]]], u_result]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove2,
			  wl:arglist_info(u_ob_c36_prove2, f_u_ob_c36_prove2, [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove2,
			  wl:init_args(exact_only, f_u_ob_c36_prove2))).
*/
/*
(cons prule (car elem))
*/
/*
(defun ob$prove-all (prove-obs bd max-number prules pfacts ignore-slots)
  (let ((bd-list (ob$prove2 (car prove-obs)
                            bd max-number prules pfacts ignore-slots)))
    (if (null? (cdr prove-obs))
        bd-list
        (yloop (yfor bd in bd-list)
               (initial (result nil))
               (ydo
                (setq result
                  (append! result (ob$prove-all (cdr prove-obs)
                                                bd max-number prules
                                                pfacts ignore-slots))))
               (yresult result)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:3657 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$prove-all',['prove-obs',bd,'max-number',prules,pfacts,'ignore-slots'],[let,[['bd-list',['ob$prove2',[car,'prove-obs'],bd,'max-number',prules,pfacts,'ignore-slots']]],[if,['null?',[cdr,'prove-obs']],'bd-list',[yloop,[yfor,bd,in,'bd-list'],[initial,[result,[]]],[ydo,[setq,result,['append!',result,['ob$prove-all',[cdr,'prove-obs'],bd,'max-number',prules,pfacts,'ignore-slots']]]],[yresult,result]]]]])
wl:lambda_def(defun, u_ob_c36_prove_all, f_u_ob_c36_prove_all, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[let, [[u_bd_list, [u_ob_c36_prove2, [car, u_prove_obs], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]], [if, [u_null_c63, [cdr, u_prove_obs]], u_bd_list, [u_yloop, [u_yfor, u_bd, u_in, u_bd_list], [u_initial, [u_result, []]], [u_ydo, [setq, u_result, [u_append_c33, u_result, [u_ob_c36_prove_all, [cdr, u_prove_obs], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]]], [u_yresult, u_result]]]]]).
wl:arglist_info(u_ob_c36_prove_all, f_u_ob_c36_prove_all, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_prove_all).

/*

### Compiled:  `U::OB$PROVE-ALL` 
*/
f_u_ob_c36_prove_all(Prove_obs, Bd, Max_number, Prules, Pfacts, Ignore_slots, FnResult) :-
	nop(global_env(Env)),
	Env24=[bv(u_prove_obs, Prove_obs), bv(u_bd, Bd), bv(u_max_number, Max_number), bv(u_prules, Prules), bv(u_pfacts, Pfacts), bv(u_ignore_slots, Ignore_slots)|Env],
	get_var(Env24, u_prove_obs, Prove_obs_Get),
	cl_car(Prove_obs_Get, C36_prove2_Param),
	get_var(Env24, u_bd, Bd_Get),
	get_var(Env24, u_ignore_slots, Ignore_slots_Get),
	get_var(Env24, u_max_number, Max_number_Get),
	get_var(Env24, u_pfacts, Pfacts_Get),
	get_var(Env24, u_prules, Prules_Get),
	f_u_ob_c36_prove2(C36_prove2_Param,
			  Bd_Get,
			  Max_number_Get,
			  Prules_Get,
			  Pfacts_Get,
			  Ignore_slots_Get,
			  Bd_list_Init),
	LEnv=[bv(u_bd_list, Bd_list_Init)|Env24],
	f_u_null_c63([cdr, u_prove_obs], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_bd_list, Bd_list_Get),
	    FnResult=Bd_list_Get
	;   f_u_yloop(
		      [ [u_yfor, u_bd, u_in, u_bd_list],
			[u_initial, [u_result, []]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ u_append_c33,
			      u_result,
			      
			      [ u_ob_c36_prove_all,
				[cdr, u_prove_obs],
				u_bd,
				u_max_number,
				u_prules,
				u_pfacts,
				u_ignore_slots
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_prove_all, classof, claz_function),
   set_opv(u_ob_c36_prove_all, compile_as, kw_function),
   set_opv(u_ob_c36_prove_all, function, f_u_ob_c36_prove_all),
   _Ignored4=u_ob_c36_prove_all.
/*
:- side_effect(assert_lsp(u_ob_c36_prove_all,
			  wl:lambda_def(defun, u_ob_c36_prove_all, f_u_ob_c36_prove_all, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[let, [[u_bd_list, [u_ob_c36_prove2, [car, u_prove_obs], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]], [if, [u_null_c63, [cdr, u_prove_obs]], u_bd_list, [u_yloop, [u_yfor, u_bd, u_in, u_bd_list], [u_initial, [u_result, []]], [u_ydo, [setq, u_result, [u_append_c33, u_result, [u_ob_c36_prove_all, [cdr, u_prove_obs], u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]]], [u_yresult, u_result]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove_all,
			  wl:arglist_info(u_ob_c36_prove_all, f_u_ob_c36_prove_all, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove_all,
			  wl:init_args(exact_only, f_u_ob_c36_prove_all))).
*/
/*
(defun ob$prove-any (prove-obs bd max-number prules pfacts ignore-slots)
  (yloop
   (initial (result nil))
   (yfor elem in prove-obs)
   (yuntil result)
   (ydo
    (setq result (ob$prove2 elem bd max-number prules pfacts ignore-slots)))
   (yresult result)))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:4259 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$prove-any',['prove-obs',bd,'max-number',prules,pfacts,'ignore-slots'],[yloop,[initial,[result,[]]],[yfor,elem,in,'prove-obs'],[yuntil,result],[ydo,[setq,result,['ob$prove2',elem,bd,'max-number',prules,pfacts,'ignore-slots']]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_prove_any, f_u_ob_c36_prove_any, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_elem, u_in, u_prove_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [u_ob_c36_prove2, u_elem, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_prove_any, f_u_ob_c36_prove_any, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_prove_any).

/*

### Compiled:  `U::OB$PROVE-ANY` 
*/
f_u_ob_c36_prove_any(Prove_obs, Bd, Max_number, Prules, Pfacts, Ignore_slots, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_prove_obs, Prove_obs), bv(u_bd, Bd), bv(u_max_number, Max_number), bv(u_prules, Prules), bv(u_pfacts, Pfacts), bv(u_ignore_slots, Ignore_slots)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_elem, u_in, u_prove_obs],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ u_ob_c36_prove2,
			  u_elem,
			  u_bd,
			  u_max_number,
			  u_prules,
			  u_pfacts,
			  u_ignore_slots
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove_any, classof, claz_function),
   set_opv(u_ob_c36_prove_any, compile_as, kw_function),
   set_opv(u_ob_c36_prove_any, function, f_u_ob_c36_prove_any),
   _Ignored4=u_ob_c36_prove_any.
/*
:- side_effect(assert_lsp(u_ob_c36_prove_any,
			  wl:lambda_def(defun, u_ob_c36_prove_any, f_u_ob_c36_prove_any, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_elem, u_in, u_prove_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [u_ob_c36_prove2, u_elem, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove_any,
			  wl:arglist_info(u_ob_c36_prove_any, f_u_ob_c36_prove_any, [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], arginfo{all:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], opt:0, req:[u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_prove_any,
			  wl:init_args(exact_only, f_u_ob_c36_prove_any))).
*/
/*
 End of file.
*/


%; Total compilation time: 1.279 seconds


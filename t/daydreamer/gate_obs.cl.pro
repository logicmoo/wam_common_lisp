#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_obs" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:15:33 2017

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
*******************************************************************************
*/
/*
*/
/*
 Type definitions for specials
*/
/*
*/
/*
(ty$create 'UVAR nil '(prop (name unifies-with) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:319 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UVAR'],[],[quote,[prop,[name,'unifies-with'],[]]]])
:- f_u_ty_c36_create(u_uvar,
		     [],
		     [u_prop, [sys_name, u_unifies_with], []],
		     _Ignored4).
/*
(ty$create 'USPECIAL nil nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:372 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'USPECIAL'],[],[]])
:- f_u_ty_c36_create(u_uspecial, [], [], _Ignored4).
/*
(ty$create 'UAND '(USPECIAL) '(prop (obj) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:402 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UAND'],[quote,['USPECIAL']],[quote,[prop,[obj],[]]]])
:- f_u_ty_c36_create(u_uand, [u_uspecial], [u_prop, [u_obj], []], _Ignored4).
/*
(ty$create 'UOR '(USPECIAL) '(prop (obj) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:449 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UOR'],[quote,['USPECIAL']],[quote,[prop,[obj],[]]]])
:- f_u_ty_c36_create(u_uor, [u_uspecial], [u_prop, [u_obj], []], _Ignored4).
/*
(ty$create 'UNOT '(USPECIAL) '(prop (obj) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:495 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UNOT'],[quote,['USPECIAL']],[quote,[prop,[obj],[]]]])
:- f_u_ty_c36_create(u_unot, [u_uspecial], [u_prop, [u_obj], []], _Ignored4).
/*
(ty$create 'UDIST '(USPECIAL) '(prop (obj) ())) ; 'distinct'
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:542 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UDIST'],[quote,['USPECIAL']],[quote,[prop,[obj],[]]]])
:- f_u_ty_c36_create(u_udist, [u_uspecial], [u_prop, [u_obj], []], _Ignored4).
/*
 'distinct'
*/
/*
(ty$create 'UPROC '(USPECIAL) '(prop (proc) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:603 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UPROC'],[quote,['USPECIAL']],[quote,[prop,[proc],[]]]])
:- f_u_ty_c36_create(u_uproc, [u_uspecial], [u_prop, [u_proc], []], _Ignored4).
/*
(ty$create 'UEMPTY-SLOTS '(USPECIAL) '(prop (slots) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:652 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UEMPTY-SLOTS'],[quote,['USPECIAL']],[quote,[prop,[slots],[]]]])
:- f_u_ty_c36_create(u_uempty_slots,
		     [u_uspecial],
		     [u_prop, [sys_slots], []],
		     _Ignored4).
/*
(ty$create 'UIGNORE-SLOTS '(USPECIAL) '(prop (slots pattern) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:709 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UIGNORE-SLOTS'],[quote,['USPECIAL']],[quote,[prop,[slots,pattern],[]]]])
:- f_u_ty_c36_create(u_uignore_slots,
		     [u_uspecial],
		     [u_prop, [sys_slots, u_pattern], []],
		     _Ignored4).
/*
(ty$create 'UPATH '(USPECIAL) '(prop (path pattern) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:775 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UPATH'],[quote,['USPECIAL']],[quote,[prop,[path,pattern],[]]]])
:- f_u_ty_c36_create(u_upath,
		     [u_uspecial],
		     [u_prop, [u_path, u_pattern], []],
		     _Ignored4).
/*
(ty$create 'UOLPATH '(USPECIAL) '(prop (link direction pattern) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:832 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UOLPATH'],[quote,['USPECIAL']],[quote,[prop,[link,direction,pattern],[]]]])
:- f_u_ty_c36_create(u_uolpath,
		     [u_uspecial],
		     [u_prop, [u_link, u_direction, u_pattern], []],
		     _Ignored4).
/*
(ty$create 'UEVAL '(USPECIAL) '(prop (proc) ()))
;
; The below are used mostly for instantiation.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:901 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UEVAL'],[quote,['USPECIAL']],[quote,[prop,[proc],[]]]])
:- f_u_ty_c36_create(u_ueval, [u_uspecial], [u_prop, [u_proc], []], _Ignored4).
/*
*/
/*
 The below are used mostly for instantiation.
*/
/*
*/
/*
(ty$create 'USELECT '(USPECIAL) '(prop (pattern slot) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1001 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'USELECT'],[quote,['USPECIAL']],[quote,[prop,[pattern,slot],[]]]])
:- f_u_ty_c36_create(u_uselect,
		     [u_uspecial],
		     [u_prop, [u_pattern, u_slot], []],
		     _Ignored4).
/*
(ty$create 'UCODE '(USPECIAL) '(prop (proc) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1060 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UCODE'],[quote,['USPECIAL']],[quote,[prop,[proc],[]]]])
:- f_u_ty_c36_create(u_ucode, [u_uspecial], [u_prop, [u_proc], []], _Ignored4).
/*
(ty$create 'UBIND! '(USPECIAL) '(prop (var pattern) ()))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1109 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'UBIND!'],[quote,['USPECIAL']],[quote,[prop,[var,pattern],[]]]])
:- f_u_ty_c36_create(u_ubind_c33,
		     [u_uspecial],
		     [u_prop, [u_var, u_pattern], []],
		     _Ignored4).
/*
(setq *special-priorities*
  (list ^UOR ^UAND ^UNOT ^UDIST ^UPROC ^UEMPTY-SLOTS
        ^UIGNORE-SLOTS ^UPATH ^UOLPATH))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1167 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*special-priorities*',[list,'^UOR','^UAND','^UNOT','^UDIST','^UPROC','^UEMPTY-SLOTS','^UIGNORE-SLOTS','^UPATH','^UOLPATH']])
:- get_var(AEnv, u_c94_uand, C94_uand_Get),
   get_var(AEnv, u_c94_udist, C94_udist_Get),
   ( get_var(AEnv, u_c94_uempty_slots, C94_uempty_slots_Get),
     get_var(AEnv, u_c94_uor, C94_uor_Get)
   ),
   ( get_var(AEnv, u_c94_uignore_slots, C94_uignore_slots_Get),
     get_var(AEnv, u_c94_unot, C94_unot_Get)
   ),
   ( get_var(AEnv, u_c94_uolpath, C94_uolpath_Get),
     get_var(AEnv, u_c94_uproc, C94_uproc_Get)
   ),
   get_var(AEnv, u_c94_upath, C94_upath_Get),
   _Ignored4=[C94_uor_Get, C94_uand_Get, C94_unot_Get, C94_udist_Get, C94_uproc_Get, C94_uempty_slots_Get, C94_uignore_slots_Get, C94_upath_Get, C94_uolpath_Get],
   set_var(AEnv, u_xx_special_priorities_xx, _Ignored4).
/*
(ty$create 'PRULE nil '(nil (subgoal goal) ()))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1289 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'PRULE'],[],[quote,[[],[subgoal,goal],[]]]])
:- f_u_ty_c36_create(u_prule, [], [[], [u_subgoal, u_goal], []], _Ignored4).
/*
(ty$create 'RULEOPER nil '(prop (obj) ()))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1338 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RULEOPER'],[],[quote,[prop,[obj],[]]]])
:- f_u_ty_c36_create(u_ruleoper, [], [u_prop, [u_obj], []], _Ignored4).
/*
(ty$create 'RAND '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1381 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RAND'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rand, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'RSEQ '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1415 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RSEQ'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rseq, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'ROR '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1449 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'ROR'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_ror, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'RNOT '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1482 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RNOT'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rnot, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'RTRUE '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1516 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RTRUE'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rtrue, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'RFALSE '(RULEOPER) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1551 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RFALSE'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rfalse, [u_ruleoper], [], _Ignored4).
/*
(ty$create 'RCODE '(RULEOPER) nil)

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl:1587 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'RCODE'],[quote,['RULEOPER']],[]])
:- f_u_ty_c36_create(u_rcode, [u_ruleoper], [], _Ignored4).
/*
 End of file.
*/


%; Total compilation time: 0.639 seconds


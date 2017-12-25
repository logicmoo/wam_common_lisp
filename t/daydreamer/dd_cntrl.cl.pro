#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "dd_cntrl" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
%; Start time: Sat Dec 23 21:22:56 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
;(break)
*/
/*
*/
/*
 Daydreamer
*/
/*
 Version 3.5
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
  9/25/86: Removed flavors
*/
/*
 19990504: ported to Common Lisp
*/
/*
*/
/*
*******************************************************************************
*/
/*
*/
/*
 Top-level functions
*/
/*
*/
/*
(setq *starting-state* 'daydreaming)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:379 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*starting-state*',[quote,daydreaming]])
:- set_var(AEnv, setq, u_xx_starting_state_xx, u_daydreaming).
/*
(defun daydreamer ()
  (ndbg-reset)
  (if *typeset?*
      (format *gate-dbg* "\\begin{flushleft}"(defun daydreamer ()\n  (ndbg-reset)\n  (if *typeset?*\n      (format *gate-dbg* \"\\\\begin{flushleft}~%\"))\n  (ndbg-roman-nl *gate-dbg* rule *dd-version*)\n  (setq *state* 'suspended)\n  (daydreamer-initialize)\n  (set-state *starting-state*)\n  ; Get off the ground by running inferences which will activate\n  ; some top-level goals.\n  (run-inferences *reality-lookahead* nil *me-belief-path*)\n  ; Run the top-level emotion-directed control loop.\n  (daydreamer-control0)\n  (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER terminates\")\n  (if *typeset?*\n      (format *gate-dbg* \"\\\\end{flushleft}~%\"))\n  t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,daydreamer,[],['ndbg-reset'],[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\begin{flushleft}~%")]],['ndbg-roman-nl','*gate-dbg*',rule,'*dd-version*'],[setq,'*state*',[quote,suspended]],['daydreamer-initialize'],['set-state','*starting-state*'],['run-inferences','*reality-lookahead*',[],'*me-belief-path*'],['daydreamer-control0'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER terminates")],[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\end{flushleft}~%")]],t])
wl:lambda_def(defun, u_daydreamer, f_u_daydreamer, [], [[u_ndbg_reset], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\begin{flushleft}~%")]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, u_xx_dd_version_xx], [setq, u_xx_state_xx, [quote, u_suspended]], [u_daydreamer_initialize], [u_set_state, u_xx_starting_state_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_daydreamer_control0], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER terminates")], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")]], t]).
wl:arglist_info(u_daydreamer, f_u_daydreamer, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_daydreamer).

/*

### Compiled:  `U::DAYDREAMER` 
*/
f_u_daydreamer(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_ndbg_reset(Ndbg_reset_Ret),
	get_var(AEnv, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 "\\begin{flushleft}~%")
		      ],
		      TrueResult),
	    _135136712=TrueResult
	;   _135136712=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  [u_xx_dd_version_xx],
			  Roman_nl_Ret),
	set_var(AEnv, setq, u_xx_state_xx, u_suspended),
	f_u_daydreamer_initialize(Daydreamer_initialize_Ret),
	get_var(AEnv, u_xx_starting_state_xx, Xx_starting_state_xx_Get),
	f_u_set_state(Xx_starting_state_xx_Get, Set_state_Ret),
	get_var(AEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(AEnv, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get,
			   [],
			   Xx_me_belief_path_xx_Get,
			   Run_inferences_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "DAYDREAMER terminates")
			  ],
			  Roman_nl_Ret26),
	get_var(AEnv, u_xx_typeset_c63_xx, IFTEST13),
	(   IFTEST13\==[]
	->  get_var(AEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get16),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get16,
			'$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")
		      ],
		      TrueResult17),
	    _135186944=TrueResult17
	;   _135186944=[]
	),
	t=FnResult.
:- set_opv(f_u_daydreamer, classof, claz_function),
   set_opv(u_daydreamer, compile_as, kw_function),
   set_opv(u_daydreamer, function, f_u_daydreamer),
   DefunResult=u_daydreamer.
/*
:- side_effect(assert_lsp(u_daydreamer,
			  wl:lambda_def(defun, u_daydreamer, f_u_daydreamer, [], [[u_ndbg_reset], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\begin{flushleft}~%")]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, u_xx_dd_version_xx], [setq, u_xx_state_xx, [quote, u_suspended]], [u_daydreamer_initialize], [u_set_state, u_xx_starting_state_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_daydreamer_control0], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER terminates")], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\end{flushleft}~%")]], t]))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer,
			  wl:arglist_info(u_daydreamer, f_u_daydreamer, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer,
			  wl:init_args(exact_only, f_u_daydreamer))).
*/
/*
 Get off the ground by running inferences which will activate
*/
/*
 some top-level goals.
*/
/*
 Run the top-level emotion-directed control loop.
*/
/*
(defun dd-continue ()
  (daydreamer-control0))

; This will pick up from the first subgoal in the set of subgoals of
; which goal is a part. We need to find the first context in which there
; is a plan for goal and back up one.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'dd-continue',[],['daydreamer-control0']])
wl:lambda_def(defun, u_dd_continue, f_u_dd_continue, [], [[u_daydreamer_control0]]).
wl:arglist_info(u_dd_continue, f_u_dd_continue, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_dd_continue).

/*

### Compiled:  `U::DD-CONTINUE` 
*/
f_u_dd_continue(FnResult) :-
	nop(global_env(Env)),
	_136846122=Env,
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_dd_continue, classof, claz_function),
   set_opv(u_dd_continue, compile_as, kw_function),
   set_opv(u_dd_continue, function, f_u_dd_continue),
   DefunResult=u_dd_continue.
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:lambda_def(defun, u_dd_continue, f_u_dd_continue, [], [[u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:arglist_info(u_dd_continue, f_u_dd_continue, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_dd_continue,
			  wl:init_args(exact_only, f_u_dd_continue))).
*/
/*
 This will pick up from the first subgoal in the set of subgoals of
*/
/*
 which goal is a part. We need to find the first context in which there
*/
/*
 is a plan for goal and back up one.
*/
/*
(defun pickup (goal)
  (let ((context (ob$get goal 'activation-context))
        (top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER "(defun pickup (goal)\n  (let ((context (ob$get goal 'activation-context))\n        (top-level-goal (ob$get goal 'top-level-goal)))\n       (ob$set context 'children nil)\n       (set-next-context top-level-goal context)\n       (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER ~A pickup\" goal)\n       (daydreamer-control0)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1238 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,pickup,[goal],[let,[[context,['ob$get',goal,[quote,'activation-context']]],['top-level-goal',['ob$get',goal,[quote,'top-level-goal']]]],['ob$set',context,[quote,children],[]],['set-next-context','top-level-goal',context],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER ~A pickup"),goal],['daydreamer-control0']]])
wl:lambda_def(defun, u_pickup, f_u_pickup, [u_goal], [[let, [[u_context, [u_ob_c36_get, u_goal, [quote, u_activation_context]]], [u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]).
wl:arglist_info(u_pickup, f_u_pickup, [u_goal], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_pickup).

/*

### Compiled:  `U::PICKUP` 
*/
f_u_pickup(Goal, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_goal, Goal)|Env],
	get_var(Env16, u_goal, Goal_Get),
	f_u_ob_c36_get(Goal_Get, u_activation_context, Context_Init),
	get_var(Env16, u_goal, Goal_Get8),
	f_u_ob_c36_get(Goal_Get8, u_top_level_goal, Top_level_goal_Init),
	LEnv=[bv(u_context, Context_Init), bv(u_top_level_goal, Top_level_goal_Init)|Env16],
	get_var(LEnv, u_context, Context_Get),
	f_u_ob_c36_set(Context_Get, u_children, [], C36_set_Ret),
	get_var(LEnv, u_context, Context_Get13),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Get13,
			     Next_context_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "DAYDREAMER ~A pickup"),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_daydreamer_control0(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_pickup, classof, claz_function),
   set_opv(u_pickup, compile_as, kw_function),
   set_opv(u_pickup, function, f_u_pickup),
   DefunResult=u_pickup.
/*
:- side_effect(assert_lsp(u_pickup,
			  wl:lambda_def(defun, u_pickup, f_u_pickup, [u_goal], [[let, [[u_context, [u_ob_c36_get, u_goal, [quote, u_activation_context]]], [u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]))).
*/
/*
:- side_effect(assert_lsp(u_pickup,
			  wl:arglist_info(u_pickup, f_u_pickup, [u_goal], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pickup, wl:init_args(exact_only, f_u_pickup))).
*/
/*
(defun pickup-in (goal context)
  (let ((top-level-goal (ob$get goal 'top-level-goal)))
       (ob$set context 'children nil)
       (set-next-context top-level-goal context)
       (ndbg-roman-nl *gate-dbg* rule "DAYDREAMER "(defun pickup-in (goal context)\n  (let ((top-level-goal (ob$get goal 'top-level-goal)))\n       (ob$set context 'children nil)\n       (set-next-context top-level-goal context)\n       (ndbg-roman-nl *gate-dbg* rule \"DAYDREAMER ~A pickup\" goal)\n       (daydreamer-control0)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1553 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'pickup-in',[goal,context],[let,[['top-level-goal',['ob$get',goal,[quote,'top-level-goal']]]],['ob$set',context,[quote,children],[]],['set-next-context','top-level-goal',context],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("DAYDREAMER ~A pickup"),goal],['daydreamer-control0']]])
wl:lambda_def(defun, u_pickup_in, f_u_pickup_in, [u_goal, u_context], [[let, [[u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]).
wl:arglist_info(u_pickup_in, f_u_pickup_in, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_pickup_in).

/*

### Compiled:  `U::PICKUP-IN` 
*/
f_u_pickup_in(Goal, Context, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_goal, Goal), bv(u_context, Context)|Env],
	get_var(Env14, u_goal, Goal_Get),
	f_u_ob_c36_get(Goal_Get, u_top_level_goal, Top_level_goal_Init),
	LEnv=[bv(u_top_level_goal, Top_level_goal_Init)|Env14],
	get_var(LEnv, u_context, Context_Get),
	f_u_ob_c36_set(Context_Get, u_children, [], C36_set_Ret),
	get_var(LEnv, u_context, Context_Get11),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Get11,
			     Next_context_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "DAYDREAMER ~A pickup"),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_daydreamer_control0(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_pickup_in, classof, claz_function),
   set_opv(u_pickup_in, compile_as, kw_function),
   set_opv(u_pickup_in, function, f_u_pickup_in),
   DefunResult=u_pickup_in.
/*
:- side_effect(assert_lsp(u_pickup_in,
			  wl:lambda_def(defun, u_pickup_in, f_u_pickup_in, [u_goal, u_context], [[let, [[u_top_level_goal, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]], [u_ob_c36_set, u_context, [quote, u_children], []], [u_set_next_context, u_top_level_goal, u_context], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "DAYDREAMER ~A pickup"), u_goal], [u_daydreamer_control0]]]))).
*/
/*
:- side_effect(assert_lsp(u_pickup_in,
			  wl:arglist_info(u_pickup_in, f_u_pickup_in, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pickup_in, wl:init_args(exact_only, f_u_pickup_in))).
*/
/*
(defun resume (goal)
  (change-tlg-status goal 'runable)
  (daydreamer-control0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1827 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,resume,[goal],['change-tlg-status',goal,[quote,runable]],['daydreamer-control0']])
wl:lambda_def(defun, u_resume, f_u_resume, [u_goal], [[u_change_tlg_status, u_goal, [quote, u_runable]], [u_daydreamer_control0]]).
wl:arglist_info(u_resume, f_u_resume, [u_goal], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_resume).

/*

### Compiled:  `U::RESUME` 
*/
f_u_resume(Goal, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_goal, Goal)|Env],
	get_var(Env7, u_goal, Goal_Get),
	f_u_change_tlg_status(Goal_Get, u_runable, Runable),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume, classof, claz_function),
   set_opv(u_resume, compile_as, kw_function),
   set_opv(u_resume, function, f_u_resume),
   DefunResult=u_resume.
/*
:- side_effect(assert_lsp(u_resume,
			  wl:lambda_def(defun, u_resume, f_u_resume, [u_goal], [[u_change_tlg_status, u_goal, [quote, u_runable]], [u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_resume,
			  wl:arglist_info(u_resume, f_u_resume, [u_goal], arginfo{all:[u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal], opt:0, req:[u_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_resume, wl:init_args(exact_only, f_u_resume))).
*/
/*
(defun resume-infs ()
  (yloop (yfor fact in *needs*)
        (ydo (cx$touch-fact *reality-lookahead* fact)))
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  (set-state 'performance)
  (daydreamer-control0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1910 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'resume-infs',[],[yloop,[yfor,fact,in,'*needs*'],[ydo,['cx$touch-fact','*reality-lookahead*',fact]]],['run-inferences','*reality-lookahead*',[],'*me-belief-path*'],['set-state',[quote,performance]],['daydreamer-control0']])
wl:lambda_def(defun, u_resume_infs, f_u_resume_infs, [], [[u_yloop, [u_yfor, u_fact, u_in, u_xx_needs_xx], [u_ydo, [u_cx_c36_touch_fact, u_xx_reality_lookahead_xx, u_fact]]], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_set_state, [quote, u_performance]], [u_daydreamer_control0]]).
wl:arglist_info(u_resume_infs, f_u_resume_infs, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_resume_infs).

/*

### Compiled:  `U::RESUME-INFS` 
*/
f_u_resume_infs(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	f_u_yloop(
		  [ [u_yfor, u_fact, u_in, u_xx_needs_xx],
		    
		    [ u_ydo,
		      [u_cx_c36_touch_fact, u_xx_reality_lookahead_xx, u_fact]
		    ]
		  ],
		  Yloop_Ret),
	get_var(GEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(GEnv, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get,
			   [],
			   Xx_me_belief_path_xx_Get,
			   Run_inferences_Ret),
	f_u_set_state(u_performance, Set_state_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume_infs, classof, claz_function),
   set_opv(u_resume_infs, compile_as, kw_function),
   set_opv(u_resume_infs, function, f_u_resume_infs),
   DefunResult=u_resume_infs.
/*
:- side_effect(assert_lsp(u_resume_infs,
			  wl:lambda_def(defun, u_resume_infs, f_u_resume_infs, [], [[u_yloop, [u_yfor, u_fact, u_in, u_xx_needs_xx], [u_ydo, [u_cx_c36_touch_fact, u_xx_reality_lookahead_xx, u_fact]]], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_set_state, [quote, u_performance]], [u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_resume_infs,
			  wl:arglist_info(u_resume_infs, f_u_resume_infs, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_resume_infs,
			  wl:init_args(exact_only, f_u_resume_infs))).
*/
/*
(defun resume-enter ()
  (enter-concepts *reality-lookahead* *me-belief-path*)
  (run-inferences *reality-lookahead* nil *me-belief-path*)
  (set-state 'performance)
  (daydreamer-control0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2133 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'resume-enter',[],['enter-concepts','*reality-lookahead*','*me-belief-path*'],['run-inferences','*reality-lookahead*',[],'*me-belief-path*'],['set-state',[quote,performance]],['daydreamer-control0']])
wl:lambda_def(defun, u_resume_enter, f_u_resume_enter, [], [[u_enter_concepts, u_xx_reality_lookahead_xx, u_xx_me_belief_path_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_set_state, [quote, u_performance]], [u_daydreamer_control0]]).
wl:arglist_info(u_resume_enter, f_u_resume_enter, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_resume_enter).

/*

### Compiled:  `U::RESUME-ENTER` 
*/
f_u_resume_enter(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(GEnv, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_enter_concepts(Xx_reality_lookahead_xx_Get,
			   Xx_me_belief_path_xx_Get,
			   Enter_concepts_Ret),
	get_var(GEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get7),
	get_var(GEnv, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get6),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get6,
			   [],
			   Xx_me_belief_path_xx_Get7,
			   Run_inferences_Ret),
	f_u_set_state(u_performance, Set_state_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume_enter, classof, claz_function),
   set_opv(u_resume_enter, compile_as, kw_function),
   set_opv(u_resume_enter, function, f_u_resume_enter),
   DefunResult=u_resume_enter.
/*
:- side_effect(assert_lsp(u_resume_enter,
			  wl:lambda_def(defun, u_resume_enter, f_u_resume_enter, [], [[u_enter_concepts, u_xx_reality_lookahead_xx, u_xx_me_belief_path_xx], [u_run_inferences, u_xx_reality_lookahead_xx, [], u_xx_me_belief_path_xx], [u_set_state, [quote, u_performance]], [u_daydreamer_control0]]))).
*/
/*
:- side_effect(assert_lsp(u_resume_enter,
			  wl:arglist_info(u_resume_enter, f_u_resume_enter, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_resume_enter,
			  wl:init_args(exact_only, f_u_resume_enter))).
*/
/*
(defun update-initial (cx)
  (yloop (yfor f in *initial-facts*)
         (ydo (cx$assert f cx))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2325 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'update-initial',[cx],[yloop,[yfor,f,in,'*initial-facts*'],[ydo,['cx$assert',f,cx]]]])
wl:lambda_def(defun, u_update_initial, f_u_update_initial, [u_cx], [[u_yloop, [u_yfor, u_f, u_in, u_xx_initial_facts_xx], [u_ydo, [u_cx_c36_assert, u_f, u_cx]]]]).
wl:arglist_info(u_update_initial, f_u_update_initial, [u_cx], arginfo{all:[u_cx], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_cx], opt:0, req:[u_cx], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_update_initial).

/*

### Compiled:  `U::UPDATE-INITIAL` 
*/
f_u_update_initial(Cx, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_cx, Cx)|Env],
	f_u_yloop(
		  [ [u_yfor, u_f, u_in, u_xx_initial_facts_xx],
		    [u_ydo, [u_cx_c36_assert, u_f, u_cx]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_update_initial, classof, claz_function),
   set_opv(u_update_initial, compile_as, kw_function),
   set_opv(u_update_initial, function, f_u_update_initial),
   DefunResult=u_update_initial.
/*
:- side_effect(assert_lsp(u_update_initial,
			  wl:lambda_def(defun, u_update_initial, f_u_update_initial, [u_cx], [[u_yloop, [u_yfor, u_f, u_in, u_xx_initial_facts_xx], [u_ydo, [u_cx_c36_assert, u_f, u_cx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_update_initial,
			  wl:arglist_info(u_update_initial, f_u_update_initial, [u_cx], arginfo{all:[u_cx], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_cx], opt:0, req:[u_cx], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_update_initial,
			  wl:init_args(exact_only, f_u_update_initial))).
*/
/*
(defun new-daydreamer ()
  (setq *first-time-initialize?* t)
  (daydreamer))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2424 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'new-daydreamer',[],[setq,'*first-time-initialize?*',t],[daydreamer]])
wl:lambda_def(defun, u_new_daydreamer, f_u_new_daydreamer, [], [[setq, u_xx_first_time_initialize_c63_xx, t], [u_daydreamer]]).
wl:arglist_info(u_new_daydreamer, f_u_new_daydreamer, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_new_daydreamer).

/*

### Compiled:  `U::NEW-DAYDREAMER` 
*/
f_u_new_daydreamer(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	set_var(AEnv, setq, u_xx_first_time_initialize_c63_xx, t),
	f_u_daydreamer(Daydreamer_Ret),
	Daydreamer_Ret=FnResult.
:- set_opv(f_u_new_daydreamer, classof, claz_function),
   set_opv(u_new_daydreamer, compile_as, kw_function),
   set_opv(u_new_daydreamer, function, f_u_new_daydreamer),
   DefunResult=u_new_daydreamer.
/*
:- side_effect(assert_lsp(u_new_daydreamer,
			  wl:lambda_def(defun, u_new_daydreamer, f_u_new_daydreamer, [], [[setq, u_xx_first_time_initialize_c63_xx, t], [u_daydreamer]]))).
*/
/*
:- side_effect(assert_lsp(u_new_daydreamer,
			  wl:arglist_info(u_new_daydreamer, f_u_new_daydreamer, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_new_daydreamer,
			  wl:init_args(exact_only, f_u_new_daydreamer))).
*/
/*
(setq *state* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2502 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*state*',[]])
:- set_var(AEnv, setq, u_xx_state_xx, []).
/*
(defun set-state (state)
  (if (not (memq? state
           '(suspended performance daydreaming)))
      (set-state (error ""(defun set-state (state)\n  (if (not (memq? state\n           '(suspended performance daydreaming)))\n      (set-state (error \"~A is not a valid state.\" state))\n      (if (neq? state *state*)\n          (progn\n           (ndbg-roman-nl *gate-dbg* rule \"State changes from ~A to ~A\"\n                          *state* state)\n           (setq *state* state)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2522 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'set-state',[state],[if,[not,['memq?',state,[quote,[suspended,performance,daydreaming]]]],['set-state',[error,'$STRING'("~A is not a valid state."),state]],[if,['neq?',state,'*state*'],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("State changes from ~A to ~A"),'*state*',state],[setq,'*state*',state]]]]])
wl:lambda_def(defun, u_set_state, f_u_set_state, [u_state], [[if, [not, [u_memq_c63, u_state, [quote, [u_suspended, u_performance, u_daydreaming]]]], [u_set_state, [error, '$ARRAY'([*], claz_base_character, "~A is not a valid state."), u_state]], [if, [u_neq_c63, u_state, u_xx_state_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "State changes from ~A to ~A"), u_xx_state_xx, u_state], [setq, u_xx_state_xx, u_state]]]]]).
wl:arglist_info(u_set_state, f_u_set_state, [u_state], arginfo{all:[u_state], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_state], opt:0, req:[u_state], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_state).

/*

### Compiled:  `U::SET-STATE` 
*/
f_u_set_state(State, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_state, State)|Env],
	f_u_memq_c63(u_state,
		     [quote, [u_suspended, u_performance, u_daydreaming]],
		     PredArgResult),
	(   PredArgResult==[]
	->  get_var(AEnv, u_state, State_Get),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"~A is not a valid state."),
		       State_Get
		     ],
		     Set_state_Param),
	    f_u_set_state(Set_state_Param, TrueResult13),
	    FnResult=TrueResult13
	;   f_u_neq_c63(u_state, u_xx_state_xx, IFTEST8),
	    (   IFTEST8\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "State changes from ~A to ~A"),
				    u_xx_state_xx,
				    u_state
				  ],
				  Roman_nl_Ret),
		get_var(AEnv, u_state, State_Get11),
		set_var(AEnv, u_xx_state_xx, State_Get11),
		FnResult=State_Get11
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_set_state, classof, claz_function),
   set_opv(u_set_state, compile_as, kw_function),
   set_opv(u_set_state, function, f_u_set_state),
   DefunResult=u_set_state.
/*
:- side_effect(assert_lsp(u_set_state,
			  wl:lambda_def(defun, u_set_state, f_u_set_state, [u_state], [[if, [not, [u_memq_c63, u_state, [quote, [u_suspended, u_performance, u_daydreaming]]]], [u_set_state, [error, '$ARRAY'([*], claz_base_character, "~A is not a valid state."), u_state]], [if, [u_neq_c63, u_state, u_xx_state_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "State changes from ~A to ~A"), u_xx_state_xx, u_state], [setq, u_xx_state_xx, u_state]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_set_state,
			  wl:arglist_info(u_set_state, f_u_set_state, [u_state], arginfo{all:[u_state], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_state], opt:0, req:[u_state], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_state, wl:init_args(exact_only, f_u_set_state))).
*/
/*
(defun performance-mode? ()
  (eq? *state* 'performance))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2878 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'performance-mode?',[],['eq?','*state*',[quote,performance]]])
wl:lambda_def(defun, u_performance_mode_c63, f_u_performance_mode_c63, [], [[u_eq_c63, u_xx_state_xx, [quote, u_performance]]]).
wl:arglist_info(u_performance_mode_c63, f_u_performance_mode_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_performance_mode_c63).

/*

### Compiled:  `U::PERFORMANCE-MODE?` 
*/
f_u_performance_mode_c63(FnResult) :-
	nop(global_env(Env)),
	_144955872=Env,
	f_u_eq_c63(u_xx_state_xx, [quote, u_performance], Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_performance_mode_c63, classof, claz_function),
   set_opv(u_performance_mode_c63, compile_as, kw_function),
   set_opv(u_performance_mode_c63, function, f_u_performance_mode_c63),
   DefunResult=u_performance_mode_c63.
/*
:- side_effect(assert_lsp(u_performance_mode_c63,
			  wl:lambda_def(defun, u_performance_mode_c63, f_u_performance_mode_c63, [], [[u_eq_c63, u_xx_state_xx, [quote, u_performance]]]))).
*/
/*
:- side_effect(assert_lsp(u_performance_mode_c63,
			  wl:arglist_info(u_performance_mode_c63, f_u_performance_mode_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_performance_mode_c63,
			  wl:init_args(exact_only, f_u_performance_mode_c63))).
*/
/*
(defun daydreaming-mode? ()
  (eq? *state* 'daydreaming))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2937 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'daydreaming-mode?',[],['eq?','*state*',[quote,daydreaming]]])
wl:lambda_def(defun, u_daydreaming_mode_c63, f_u_daydreaming_mode_c63, [], [[u_eq_c63, u_xx_state_xx, [quote, u_daydreaming]]]).
wl:arglist_info(u_daydreaming_mode_c63, f_u_daydreaming_mode_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_daydreaming_mode_c63).

/*

### Compiled:  `U::DAYDREAMING-MODE?` 
*/
f_u_daydreaming_mode_c63(FnResult) :-
	nop(global_env(Env)),
	_145499684=Env,
	f_u_eq_c63(u_xx_state_xx, [quote, u_daydreaming], Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_daydreaming_mode_c63, classof, claz_function),
   set_opv(u_daydreaming_mode_c63, compile_as, kw_function),
   set_opv(u_daydreaming_mode_c63, function, f_u_daydreaming_mode_c63),
   DefunResult=u_daydreaming_mode_c63.
/*
:- side_effect(assert_lsp(u_daydreaming_mode_c63,
			  wl:lambda_def(defun, u_daydreaming_mode_c63, f_u_daydreaming_mode_c63, [], [[u_eq_c63, u_xx_state_xx, [quote, u_daydreaming]]]))).
*/
/*
:- side_effect(assert_lsp(u_daydreaming_mode_c63,
			  wl:arglist_info(u_daydreaming_mode_c63, f_u_daydreaming_mode_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreaming_mode_c63,
			  wl:init_args(exact_only, f_u_daydreaming_mode_c63))).
*/
/*
(setq *reality* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2996 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*reality*',[]])
:- set_var(AEnv, setq, u_xx_reality_xx, []).
/*
(setq *reality-lookahead* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3017 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*reality-lookahead*',[]])
:- set_var(AEnv, setq, u_xx_reality_lookahead_xx, []).
/*
(setq *primal-reality* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3048 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*primal-reality*',[]])
:- set_var(AEnv, setq, u_xx_primal_reality_xx, []).
/*
(setq *first-time-initialize?* t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3076 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*first-time-initialize?*',t])
:- set_var(AEnv, setq, u_xx_first_time_initialize_c63_xx, t).
/*
(setq *initial-reality* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3111 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*initial-reality*',[]])
:- set_var(AEnv, setq, u_xx_initial_reality_xx, []).
/*
(setq *entered-concepts* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3141 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*entered-concepts*',[]])
:- set_var(AEnv, setq, u_xx_entered_concepts_xx, []).
/*
(defun daydreamer-initialize ()
  (ndbg-roman-nl *gate-dbg* rule "Initialize DAYDREAMER")
  (if *first-time-initialize?*
      (first-time-initialize))
  (ndbg-roman-nl *gate-dbg* rule ""(defun daydreamer-initialize ()\n  (ndbg-roman-nl *gate-dbg* rule \"Initialize DAYDREAMER\")\n  (if *first-time-initialize?*\n      (first-time-initialize))\n  (ndbg-roman-nl *gate-dbg* rule \"~%Creating initial reality context...\")\n  (setq *top-level-goals* nil)\n  (setq *top-level-goal* nil)\n  (setq *emotions* nil)\n  (setq *reality*\n        (cx$sprout *primal-reality*))\n  (setq *initial-reality* *reality*) ; for debugging\n  (setq *entered-concepts* nil)\n  (setq *reality-lookahead* *reality*)\n  (need-init *reality*)\n  (epmem-initialize))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3172 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'daydreamer-initialize',[],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Initialize DAYDREAMER")],[if,'*first-time-initialize?*',['first-time-initialize']],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("~%Creating initial reality context...")],[setq,'*top-level-goals*',[]],[setq,'*top-level-goal*',[]],[setq,'*emotions*',[]],[setq,'*reality*',['cx$sprout','*primal-reality*']],[setq,'*initial-reality*','*reality*'],[setq,'*entered-concepts*',[]],[setq,'*reality-lookahead*','*reality*'],['need-init','*reality*'],['epmem-initialize']])
wl:lambda_def(defun, u_daydreamer_initialize, f_u_daydreamer_initialize, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Initialize DAYDREAMER")], [if, u_xx_first_time_initialize_c63_xx, [u_first_time_initialize]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~%Creating initial reality context...")], [setq, u_xx_top_level_goals_xx, []], [setq, u_xx_top_level_goal_xx, []], [setq, u_xx_emotions_xx, []], [setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_primal_reality_xx]], [setq, u_xx_initial_reality_xx, u_xx_reality_xx], [setq, u_xx_entered_concepts_xx, []], [setq, u_xx_reality_lookahead_xx, u_xx_reality_xx], [u_need_init, u_xx_reality_xx], [u_epmem_initialize]]).
wl:arglist_info(u_daydreamer_initialize, f_u_daydreamer_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_daydreamer_initialize).

/*

### Compiled:  `U::DAYDREAMER-INITIALIZE` 
*/
f_u_daydreamer_initialize(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Initialize DAYDREAMER")
			  ],
			  Roman_nl_Ret),
	get_var(AEnv, u_xx_first_time_initialize_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_first_time_initialize(TrueResult),
	    _147168204=TrueResult
	;   _147168204=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "~%Creating initial reality context...")
			  ],
			  Roman_nl_Ret17),
	set_var(AEnv, setq, u_xx_top_level_goals_xx, []),
	set_var(AEnv, setq, u_xx_top_level_goal_xx, []),
	set_var(AEnv, setq, u_xx_emotions_xx, []),
	get_var(AEnv, u_xx_primal_reality_xx, Xx_primal_reality_xx_Get),
	f_u_cx_c36_sprout(Xx_primal_reality_xx_Get, Xx_reality_xx),
	set_var(AEnv, u_xx_reality_xx, Xx_reality_xx),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	set_var(AEnv, u_xx_initial_reality_xx, Xx_reality_xx_Get),
	set_var(AEnv, setq, u_xx_entered_concepts_xx, []),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get11),
	set_var(AEnv, u_xx_reality_lookahead_xx, Xx_reality_xx_Get11),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get12),
	f_u_need_init(Xx_reality_xx_Get12, Need_init_Ret),
	f_u_epmem_initialize(Epmem_initialize_Ret),
	Epmem_initialize_Ret=FnResult.
:- set_opv(f_u_daydreamer_initialize, classof, claz_function),
   set_opv(u_daydreamer_initialize, compile_as, kw_function),
   set_opv(u_daydreamer_initialize, function, f_u_daydreamer_initialize),
   DefunResult=u_daydreamer_initialize.
/*
:- side_effect(assert_lsp(u_daydreamer_initialize,
			  wl:lambda_def(defun, u_daydreamer_initialize, f_u_daydreamer_initialize, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Initialize DAYDREAMER")], [if, u_xx_first_time_initialize_c63_xx, [u_first_time_initialize]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~%Creating initial reality context...")], [setq, u_xx_top_level_goals_xx, []], [setq, u_xx_top_level_goal_xx, []], [setq, u_xx_emotions_xx, []], [setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_primal_reality_xx]], [setq, u_xx_initial_reality_xx, u_xx_reality_xx], [setq, u_xx_entered_concepts_xx, []], [setq, u_xx_reality_lookahead_xx, u_xx_reality_xx], [u_need_init, u_xx_reality_xx], [u_epmem_initialize]]))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_initialize,
			  wl:arglist_info(u_daydreamer_initialize, f_u_daydreamer_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_initialize,
			  wl:init_args(exact_only, f_u_daydreamer_initialize))).
*/
/*
 for debugging
*/
/*
(defun dbg-info ()
  (cx$tree-print *initial-reality*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3710 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'dbg-info',[],['cx$tree-print','*initial-reality*']])
wl:lambda_def(defun, u_dbg_info, f_u_dbg_info, [], [[u_cx_c36_tree_print, u_xx_initial_reality_xx]]).
wl:arglist_info(u_dbg_info, f_u_dbg_info, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_dbg_info).

/*

### Compiled:  `U::DBG-INFO` 
*/
f_u_dbg_info(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, u_xx_initial_reality_xx, Xx_initial_reality_xx_Get),
	f_u_cx_c36_tree_print(Xx_initial_reality_xx_Get, Tree_print_Ret),
	Tree_print_Ret=FnResult.
:- set_opv(f_u_dbg_info, classof, claz_function),
   set_opv(u_dbg_info, compile_as, kw_function),
   set_opv(u_dbg_info, function, f_u_dbg_info),
   DefunResult=u_dbg_info.
/*
:- side_effect(assert_lsp(u_dbg_info,
			  wl:lambda_def(defun, u_dbg_info, f_u_dbg_info, [], [[u_cx_c36_tree_print, u_xx_initial_reality_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_dbg_info,
			  wl:arglist_info(u_dbg_info, f_u_dbg_info, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_dbg_info, wl:init_args(exact_only, f_u_dbg_info))).
*/
/*
(defun first-time-initialize ()
  (ndbg-roman-nl *gate-dbg* rule "Performing first-time initialization")
  (no-gen (initialize-primal-reality))
  (setq *first-time-initialize?* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3767 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'first-time-initialize',[],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Performing first-time initialization")],['no-gen',['initialize-primal-reality']],[setq,'*first-time-initialize?*',[]]])
wl:lambda_def(defun, u_first_time_initialize, f_u_first_time_initialize, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Performing first-time initialization")], [u_no_gen, [u_initialize_primal_reality]], [setq, u_xx_first_time_initialize_c63_xx, []]]).
wl:arglist_info(u_first_time_initialize, f_u_first_time_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_first_time_initialize).

/*

### Compiled:  `U::FIRST-TIME-INITIALIZE` 
*/
f_u_first_time_initialize(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Performing first-time initialization")
			  ],
			  Roman_nl_Ret),
	f_u_no_gen([[u_initialize_primal_reality]], No_gen_Ret),
	set_var(AEnv, setq, u_xx_first_time_initialize_c63_xx, []),
	[]=FnResult.
:- set_opv(f_u_first_time_initialize, classof, claz_function),
   set_opv(u_first_time_initialize, compile_as, kw_function),
   set_opv(u_first_time_initialize, function, f_u_first_time_initialize),
   DefunResult=u_first_time_initialize.
/*
:- side_effect(assert_lsp(u_first_time_initialize,
			  wl:lambda_def(defun, u_first_time_initialize, f_u_first_time_initialize, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Performing first-time initialization")], [u_no_gen, [u_initialize_primal_reality]], [setq, u_xx_first_time_initialize_c63_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_first_time_initialize,
			  wl:arglist_info(u_first_time_initialize, f_u_first_time_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_first_time_initialize,
			  wl:init_args(exact_only, f_u_first_time_initialize))).
*/
/*
(setq *initial-facts* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3951 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*initial-facts*',[]])
:- set_var(AEnv, setq, u_xx_initial_facts_xx, []).
/*
(defun loadable-subsets? (subsets)
  (or (nil? subsets)
      (any? (lambda (x) (memq? x *subsets*)) subsets)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3979 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'loadable-subsets?',[subsets],[or,['nil?',subsets],['any?',[lambda,[x],['memq?',x,'*subsets*']],subsets]]])
wl:lambda_def(defun, u_loadable_subsets_c63, f_u_loadable_subsets_c63, [u_subsets], [[or, [u_nil_c63, u_subsets], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_subsets_xx]], u_subsets]]]).
wl:arglist_info(u_loadable_subsets_c63, f_u_loadable_subsets_c63, [u_subsets], arginfo{all:[u_subsets], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subsets], opt:0, req:[u_subsets], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_loadable_subsets_c63).

/*

### Compiled:  `U::LOADABLE-SUBSETS?` 
*/
f_u_loadable_subsets_c63(Subsets, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_subsets, Subsets)|Env],
	(   f_u_nil_c63(u_subsets, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_any_c63([lambda, [u_x], [u_memq_c63, u_x, u_xx_subsets_xx]],
			u_subsets,
			Subsets9),
	    FnResult=Subsets9
	).
:- set_opv(f_u_loadable_subsets_c63, classof, claz_function),
   set_opv(u_loadable_subsets_c63, compile_as, kw_function),
   set_opv(u_loadable_subsets_c63, function, f_u_loadable_subsets_c63),
   DefunResult=u_loadable_subsets_c63.
/*
:- side_effect(assert_lsp(u_loadable_subsets_c63,
			  wl:lambda_def(defun, u_loadable_subsets_c63, f_u_loadable_subsets_c63, [u_subsets], [[or, [u_nil_c63, u_subsets], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_subsets_xx]], u_subsets]]]))).
*/
/*
:- side_effect(assert_lsp(u_loadable_subsets_c63,
			  wl:arglist_info(u_loadable_subsets_c63, f_u_loadable_subsets_c63, [u_subsets], arginfo{all:[u_subsets], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subsets], opt:0, req:[u_subsets], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_loadable_subsets_c63,
			  wl:init_args(exact_only, f_u_loadable_subsets_c63))).
*/
/*
(defun initialize-primal-reality ()
  (ndbg-roman-nl *gate-dbg* rule "Creating primal reality...")
  (setq *primal-reality* (cx$create))
  (yloop (yfor assertion in *initial-facts*)
         (ydo (cx$assert *primal-reality* assertion))))

;
; Top-level control loop: repeatedly select the most highly motivated
; available top-level goal and run that goal.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'initialize-primal-reality',[],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Creating primal reality...")],[setq,'*primal-reality*',['cx$create']],[yloop,[yfor,assertion,in,'*initial-facts*'],[ydo,['cx$assert','*primal-reality*',assertion]]]])
wl:lambda_def(defun, u_initialize_primal_reality, f_u_initialize_primal_reality, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Creating primal reality...")], [setq, u_xx_primal_reality_xx, [u_cx_c36_create]], [u_yloop, [u_yfor, u_assertion, u_in, u_xx_initial_facts_xx], [u_ydo, [u_cx_c36_assert, u_xx_primal_reality_xx, u_assertion]]]]).
wl:arglist_info(u_initialize_primal_reality, f_u_initialize_primal_reality, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_initialize_primal_reality).

/*

### Compiled:  `U::INITIALIZE-PRIMAL-REALITY` 
*/
f_u_initialize_primal_reality(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Creating primal reality...")
			  ],
			  Roman_nl_Ret),
	f_u_cx_c36_create(Xx_primal_reality_xx),
	set_var(AEnv, u_xx_primal_reality_xx, Xx_primal_reality_xx),
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_xx_initial_facts_xx],
		    
		    [ u_ydo,
		      [u_cx_c36_assert, u_xx_primal_reality_xx, u_assertion]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_initialize_primal_reality, classof, claz_function),
   set_opv(u_initialize_primal_reality, compile_as, kw_function),
   set_opv(u_initialize_primal_reality, function, f_u_initialize_primal_reality),
   DefunResult=u_initialize_primal_reality.
/*
:- side_effect(assert_lsp(u_initialize_primal_reality,
			  wl:lambda_def(defun, u_initialize_primal_reality, f_u_initialize_primal_reality, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Creating primal reality...")], [setq, u_xx_primal_reality_xx, [u_cx_c36_create]], [u_yloop, [u_yfor, u_assertion, u_in, u_xx_initial_facts_xx], [u_ydo, [u_cx_c36_assert, u_xx_primal_reality_xx, u_assertion]]]]))).
*/
/*
:- side_effect(assert_lsp(u_initialize_primal_reality,
			  wl:arglist_info(u_initialize_primal_reality, f_u_initialize_primal_reality, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_initialize_primal_reality,
			  wl:init_args(exact_only, f_u_initialize_primal_reality))).
*/
/*
*/
/*
 Top-level control loop: repeatedly select the most highly motivated
*/
/*
 available top-level goal and run that goal.
*/
/*
*/
/*
(defun daydreamer-control0 ()
  (ndbg-roman-nl *gate-dbg* rule "Running emotion-driven control loop...")
  (yloop (initial (candidates nil)
                 (strikes 0)
                 (top-level-goal nil))
        (yuntil (> strikes 2))
        (ydo
            (need-decay)
            (emotion-decay)
            (setq candidates (most-highly-motivated-goals))
            (format (standard-output) ":")
            (force-output (standard-output))
            (cond
             ((null? candidates)
              (if (performance-mode?)
                  (progn
                  (ndbg-roman-nl *gate-dbg* rule
                   "No more goals to run; switching to daydreaming mode")
                   (setq strikes (+ 1 strikes))
                   (set-state 'daydreaming))
                  (progn
                   (if (null? (environmental-object-input))
                       (progn
                        (ndbg-roman-nl *gate-dbg* rule
                         "No more goals to run; switching to performance mode")
                        (setq strikes (+ 1 strikes))
                        (yloop (yfor goal in *top-level-goals*)
                              (ydo (if (and (eq? 'waiting (ob$get goal 'status))
                                           (eq? 'real (ob$get goal
                                                               'planning-type)))
                                      (progn
                                       (change-tlg-status goal 'runable)))))
                        (set-state 'performance))))))
             ((memq? top-level-goal candidates)
              (setq strikes 0)
              (daydreamer-control1 top-level-goal))
             ((= (length candidates) 1)
              (setq strikes 0)
              (setq top-level-goal (car candidates))
              (ndbg-roman *gate-dbg* rule "Switching to new top-level goal")
              (ndbg-roman *gate-dbg* rule " "(defun daydreamer-control0 ()\n  (ndbg-roman-nl *gate-dbg* rule \"Running emotion-driven control loop...\")\n  (yloop (initial (candidates nil)\n                 (strikes 0)\n                 (top-level-goal nil))\n        (yuntil (> strikes 2))\n        (ydo\n            (need-decay)\n            (emotion-decay)\n            (setq candidates (most-highly-motivated-goals))\n            (format (standard-output) \":\")\n            (force-output (standard-output))\n            (cond\n             ((null? candidates)\n              (if (performance-mode?)\n                  (progn\n                  (ndbg-roman-nl *gate-dbg* rule\n                   \"No more goals to run; switching to daydreaming mode\")\n                   (setq strikes (+ 1 strikes))\n                   (set-state 'daydreaming))\n                  (progn\n                   (if (null? (environmental-object-input))\n                       (progn\n                        (ndbg-roman-nl *gate-dbg* rule\n                         \"No more goals to run; switching to performance mode\")\n                        (setq strikes (+ 1 strikes))\n                        (yloop (yfor goal in *top-level-goals*)\n                              (ydo (if (and (eq? 'waiting (ob$get goal 'status))\n                                           (eq? 'real (ob$get goal\n                                                               'planning-type)))\n                                      (progn\n                                       (change-tlg-status goal 'runable)))))\n                        (set-state 'performance))))))\n             ((memq? top-level-goal candidates)\n              (setq strikes 0)\n              (daydreamer-control1 top-level-goal))\n             ((= (length candidates) 1)\n              (setq strikes 0)\n              (setq top-level-goal (car candidates))\n              (ndbg-roman *gate-dbg* rule \"Switching to new top-level goal\")\n              (ndbg-roman *gate-dbg* rule \" ~A\" top-level-goal)\n              (ndbg-newline *gate-dbg* rule)\n              (daydreamer-control1 top-level-goal))\n             (else\n              (setq strikes 0)\n              (setq top-level-goal (random-element candidates))\n              (ndbg-roman *gate-dbg* rule \"Switching to new top-level goal\n                          (broke tie)\")\n              (ndbg-roman *gate-dbg* rule \" ~A\" top-level-goal)\n              (ndbg-newline *gate-dbg* rule)\n              (daydreamer-control1 top-level-goal))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4451 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'daydreamer-control0',[],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Running emotion-driven control loop...")],[yloop,[initial,[candidates,[]],[strikes,0],['top-level-goal',[]]],[yuntil,[>,strikes,2]],[ydo,['need-decay'],['emotion-decay'],[setq,candidates,['most-highly-motivated-goals']],[format,['standard-output'],'$STRING'(":")],['force-output',['standard-output']],[cond,[['null?',candidates],[if,['performance-mode?'],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("No more goals to run; switching to daydreaming mode")],[setq,strikes,[+,1,strikes]],['set-state',[quote,daydreaming]]],[progn,[if,['null?',['environmental-object-input']],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("No more goals to run; switching to performance mode")],[setq,strikes,[+,1,strikes]],[yloop,[yfor,goal,in,'*top-level-goals*'],[ydo,[if,[and,['eq?',[quote,waiting],['ob$get',goal,[quote,status]]],['eq?',[quote,real],['ob$get',goal,[quote,'planning-type']]]],[progn,['change-tlg-status',goal,[quote,runable]]]]]],['set-state',[quote,performance]]]]]]],[['memq?','top-level-goal',candidates],[setq,strikes,0],['daydreamer-control1','top-level-goal']],[[=,[length,candidates],1],[setq,strikes,0],[setq,'top-level-goal',[car,candidates]],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Switching to new top-level goal")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A"),'top-level-goal'],['ndbg-newline','*gate-dbg*',rule],['daydreamer-control1','top-level-goal']],[else,[setq,strikes,0],[setq,'top-level-goal',['random-element',candidates]],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Switching to new top-level goal\n                          (broke tie)")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A"),'top-level-goal'],['ndbg-newline','*gate-dbg*',rule],['daydreamer-control1','top-level-goal']]]]]])
wl:lambda_def(defun, u_daydreamer_control0, f_u_daydreamer_control0, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Running emotion-driven control loop...")], [u_yloop, [u_initial, [u_candidates, []], [u_strikes, 0], [u_top_level_goal, []]], [u_yuntil, [>, u_strikes, 2]], [u_ydo, [u_need_decay], [u_emotion_decay], [setq, u_candidates, [u_most_highly_motivated_goals]], [format, [u_standard_output], '$ARRAY'([*], claz_base_character, ":")], [force_output, [u_standard_output]], [cond, [[u_null_c63, u_candidates], [if, [u_performance_mode_c63], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "No more goals to run; switching to daydreaming mode")], [setq, u_strikes, [+, 1, u_strikes]], [u_set_state, [quote, u_daydreaming]]], [progn, [if, [u_null_c63, [u_environmental_object_input]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "No more goals to run; switching to performance mode")], [setq, u_strikes, [+, 1, u_strikes]], [u_yloop, [u_yfor, u_goal, u_in, u_xx_top_level_goals_xx], [u_ydo, [if, [and, [u_eq_c63, [quote, u_waiting], [u_ob_c36_get, u_goal, [quote, u_status]]], [u_eq_c63, [quote, real], [u_ob_c36_get, u_goal, [quote, u_planning_type]]]], [progn, [u_change_tlg_status, u_goal, [quote, u_runable]]]]]], [u_set_state, [quote, u_performance]]]]]]], [[u_memq_c63, u_top_level_goal, u_candidates], [setq, u_strikes, 0], [u_daydreamer_control1, u_top_level_goal]], [[=, [length, u_candidates], 1], [setq, u_strikes, 0], [setq, u_top_level_goal, [car, u_candidates]], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Switching to new top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_daydreamer_control1, u_top_level_goal]], [u_else, [setq, u_strikes, 0], [setq, u_top_level_goal, [u_random_element, u_candidates]], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Switching to new top-level goal\n                          (broke tie)")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_daydreamer_control1, u_top_level_goal]]]]]]).
wl:arglist_info(u_daydreamer_control0, f_u_daydreamer_control0, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_daydreamer_control0).

/*

### Compiled:  `U::DAYDREAMER-CONTROL0` 
*/
f_u_daydreamer_control0(FnResult) :-
	nop(global_env(Env)),
	_152213090=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Running emotion-driven control loop...")
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_candidates, []],
		      [u_strikes, 0],
		      [u_top_level_goal, []]
		    ],
		    [u_yuntil, [>, u_strikes, 2]],
		    
		    [ u_ydo,
		      [u_need_decay],
		      [u_emotion_decay],
		      [setq, u_candidates, [u_most_highly_motivated_goals]],
		      
		      [ format,
			[u_standard_output],
			'$ARRAY'([*], claz_base_character, ":")
		      ],
		      [force_output, [u_standard_output]],
		      
		      [ cond,
			
			[ [u_null_c63, u_candidates],
			  
			  [ if,
			    [u_performance_mode_c63],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 "No more goals to run; switching to daydreaming mode")
			      ],
			      [setq, u_strikes, [+, 1, u_strikes]],
			      [u_set_state, [quote, u_daydreaming]]
			    ],
			    
			    [ progn,
			      
			      [ if,
				[u_null_c63, [u_environmental_object_input]],
				
				[ progn,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_rule,
				    '$ARRAY'([*],
					     claz_base_character,
					     "No more goals to run; switching to performance mode")
				  ],
				  [setq, u_strikes, [+, 1, u_strikes]],
				  
				  [ u_yloop,
				    
				    [ u_yfor,
				      u_goal,
				      u_in,
				      u_xx_top_level_goals_xx
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ and,
					  
					  [ u_eq_c63,
					    [quote, u_waiting],
					    
					    [ u_ob_c36_get,
					      u_goal,
					      [quote, u_status]
					    ]
					  ],
					  
					  [ u_eq_c63,
					    [quote, real],
					    
					    [ u_ob_c36_get,
					      u_goal,
					      [quote, u_planning_type]
					    ]
					  ]
					],
					
					[ progn,
					  
					  [ u_change_tlg_status,
					    u_goal,
					    [quote, u_runable]
					  ]
					]
				      ]
				    ]
				  ],
				  [u_set_state, [quote, u_performance]]
				]
			      ]
			    ]
			  ]
			],
			
			[ [u_memq_c63, u_top_level_goal, u_candidates],
			  [setq, u_strikes, 0],
			  [u_daydreamer_control1, u_top_level_goal]
			],
			
			[ [=, [length, u_candidates], 1],
			  [setq, u_strikes, 0],
			  [setq, u_top_level_goal, [car, u_candidates]],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     "Switching to new top-level goal")
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*], claz_base_character, " ~A"),
			    u_top_level_goal
			  ],
			  [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			  [u_daydreamer_control1, u_top_level_goal]
			],
			
			[ u_else,
			  [setq, u_strikes, 0],
			  
			  [ setq,
			    u_top_level_goal,
			    [u_random_element, u_candidates]
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     "Switching to new top-level goal\n                          (broke tie)")
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*], claz_base_character, " ~A"),
			    u_top_level_goal
			  ],
			  [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			  [u_daydreamer_control1, u_top_level_goal]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_daydreamer_control0, classof, claz_function),
   set_opv(u_daydreamer_control0, compile_as, kw_function),
   set_opv(u_daydreamer_control0, function, f_u_daydreamer_control0),
   DefunResult=u_daydreamer_control0.
/*
:- side_effect(assert_lsp(u_daydreamer_control0,
			  wl:lambda_def(defun, u_daydreamer_control0, f_u_daydreamer_control0, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Running emotion-driven control loop...")], [u_yloop, [u_initial, [u_candidates, []], [u_strikes, 0], [u_top_level_goal, []]], [u_yuntil, [>, u_strikes, 2]], [u_ydo, [u_need_decay], [u_emotion_decay], [setq, u_candidates, [u_most_highly_motivated_goals]], [format, [u_standard_output], '$ARRAY'([*], claz_base_character, ":")], [force_output, [u_standard_output]], [cond, [[u_null_c63, u_candidates], [if, [u_performance_mode_c63], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "No more goals to run; switching to daydreaming mode")], [setq, u_strikes, [+, 1, u_strikes]], [u_set_state, [quote, u_daydreaming]]], [progn, [if, [u_null_c63, [u_environmental_object_input]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "No more goals to run; switching to performance mode")], [setq, u_strikes, [+, 1, u_strikes]], [u_yloop, [u_yfor, u_goal, u_in, u_xx_top_level_goals_xx], [u_ydo, [if, [and, [u_eq_c63, [quote, u_waiting], [u_ob_c36_get, u_goal, [quote, u_status]]], [u_eq_c63, [quote, real], [u_ob_c36_get, u_goal, [quote, u_planning_type]]]], [progn, [u_change_tlg_status, u_goal, [quote, u_runable]]]]]], [u_set_state, [quote, u_performance]]]]]]], [[u_memq_c63, u_top_level_goal, u_candidates], [setq, u_strikes, 0], [u_daydreamer_control1, u_top_level_goal]], [[=, [length, u_candidates], 1], [setq, u_strikes, 0], [setq, u_top_level_goal, [car, u_candidates]], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Switching to new top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_daydreamer_control1, u_top_level_goal]], [u_else, [setq, u_strikes, 0], [setq, u_top_level_goal, [u_random_element, u_candidates]], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Switching to new top-level goal\n                          (broke tie)")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_daydreamer_control1, u_top_level_goal]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_control0,
			  wl:arglist_info(u_daydreamer_control0, f_u_daydreamer_control0, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_control0,
			  wl:init_args(exact_only, f_u_daydreamer_control0))).
*/
/*
(setq *need-decay-factor* .98)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:6894 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*need-decay-factor*',0.98])
:- set_var(AEnv, setq, u_xx_need_decay_factor_xx, 0.98).
/*
(defun need-decay ()
  (ndbg-roman-nl *gate-dbg* rule-long "Need decay.")
  (yloop (yfor need in *needs*)
        (ydo (set-strength need (fl* *need-decay-factor* (strength need)))
             (cx$touch-fact *reality* need))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:6926 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'need-decay',[],['ndbg-roman-nl','*gate-dbg*','rule-long','$STRING'("Need decay.")],[yloop,[yfor,need,in,'*needs*'],[ydo,['set-strength',need,['fl*','*need-decay-factor*',[strength,need]]],['cx$touch-fact','*reality*',need]]]])
wl:lambda_def(defun, u_need_decay, f_u_need_decay, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Need decay.")], [u_yloop, [u_yfor, u_need, u_in, u_xx_needs_xx], [u_ydo, [u_set_strength, u_need, [u_fl_xx, u_xx_need_decay_factor_xx, [u_strength, u_need]]], [u_cx_c36_touch_fact, u_xx_reality_xx, u_need]]]]).
wl:arglist_info(u_need_decay, f_u_need_decay, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_need_decay).

/*

### Compiled:  `U::NEED-DECAY` 
*/
f_u_need_decay(FnResult) :-
	nop(global_env(Env)),
	_156355476=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  ['$ARRAY'([*], claz_base_character, "Need decay.")],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_yfor, u_need, u_in, u_xx_needs_xx],
		    
		    [ u_ydo,
		      
		      [ u_set_strength,
			u_need,
			
			[ u_fl_xx,
			  u_xx_need_decay_factor_xx,
			  [u_strength, u_need]
			]
		      ],
		      [u_cx_c36_touch_fact, u_xx_reality_xx, u_need]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_need_decay, classof, claz_function),
   set_opv(u_need_decay, compile_as, kw_function),
   set_opv(u_need_decay, function, f_u_need_decay),
   DefunResult=u_need_decay.
/*
:- side_effect(assert_lsp(u_need_decay,
			  wl:lambda_def(defun, u_need_decay, f_u_need_decay, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Need decay.")], [u_yloop, [u_yfor, u_need, u_in, u_xx_needs_xx], [u_ydo, [u_set_strength, u_need, [u_fl_xx, u_xx_need_decay_factor_xx, [u_strength, u_need]]], [u_cx_c36_touch_fact, u_xx_reality_xx, u_need]]]]))).
*/
/*
:- side_effect(assert_lsp(u_need_decay,
			  wl:arglist_info(u_need_decay, f_u_need_decay, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_need_decay,
			  wl:init_args(exact_only, f_u_need_decay))).
*/
/*
(setq *emotion-decay-factor* .95)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7155 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*emotion-decay-factor*',0.95])
:- set_var(AEnv, setq, u_xx_emotion_decay_factor_xx, 0.95).
/*
(setq *emotion-gc-threshold* 0.15)

; Only non-motivating emotions are subject to decay.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7190 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*emotion-gc-threshold*',0.15])
:- set_var(AEnv, setq, u_xx_emotion_gc_threshold_xx, 0.15).
/*
 Only non-motivating emotions are subject to decay.
*/
/*
(defun emotion-decay ()
  (ndbg-roman-nl *gate-dbg* rule-long "Emotion decay.")
  (yloop
   (yfor emot in *emotions*)
   (ydo
    (if (not (motivating-emotion? emot))
        (progn
         (set-strength emot (fl* *emotion-decay-factor* (strength emot)))
         (cx$touch-fact *reality* emot)
         (if (fl< (strength emot) *emotion-gc-threshold*)
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Emotion "(defun emotion-decay ()\n  (ndbg-roman-nl *gate-dbg* rule-long \"Emotion decay.\")\n  (yloop\n   (yfor emot in *emotions*)\n   (ydo\n    (if (not (motivating-emotion? emot))\n        (progn\n         (set-strength emot (fl* *emotion-decay-factor* (strength emot)))\n         (cx$touch-fact *reality* emot)\n         (if (fl< (strength emot) *emotion-gc-threshold*)\n             (progn\n              (ndbg-roman-nl *gate-dbg* rule \"Emotion ~A below threshold.\" emot)\n              (cx$retract *reality* emot)\n              (emotion-delete emot))))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7279 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'emotion-decay',[],['ndbg-roman-nl','*gate-dbg*','rule-long','$STRING'("Emotion decay.")],[yloop,[yfor,emot,in,'*emotions*'],[ydo,[if,[not,['motivating-emotion?',emot]],[progn,['set-strength',emot,['fl*','*emotion-decay-factor*',[strength,emot]]],['cx$touch-fact','*reality*',emot],[if,['fl<',[strength,emot],'*emotion-gc-threshold*'],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Emotion ~A below threshold."),emot],['cx$retract','*reality*',emot],['emotion-delete',emot]]]]]]]])
wl:lambda_def(defun, u_emotion_decay, f_u_emotion_decay, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Emotion decay.")], [u_yloop, [u_yfor, u_emot, u_in, u_xx_emotions_xx], [u_ydo, [if, [not, [u_motivating_emotion_c63, u_emot]], [progn, [u_set_strength, u_emot, [u_fl_xx, u_xx_emotion_decay_factor_xx, [u_strength, u_emot]]], [u_cx_c36_touch_fact, u_xx_reality_xx, u_emot], [if, [u_fl_c60, [u_strength, u_emot], u_xx_emotion_gc_threshold_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Emotion ~A below threshold."), u_emot], [u_cx_c36_retract, u_xx_reality_xx, u_emot], [u_emotion_delete, u_emot]]]]]]]]).
wl:arglist_info(u_emotion_decay, f_u_emotion_decay, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_emotion_decay).

/*

### Compiled:  `U::EMOTION-DECAY` 
*/
f_u_emotion_decay(FnResult) :-
	nop(global_env(Env)),
	_157549530=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  ['$ARRAY'([*], claz_base_character, "Emotion decay.")],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_yfor, u_emot, u_in, u_xx_emotions_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_motivating_emotion_c63, u_emot]],
			
			[ progn,
			  
			  [ u_set_strength,
			    u_emot,
			    
			    [ u_fl_xx,
			      u_xx_emotion_decay_factor_xx,
			      [u_strength, u_emot]
			    ]
			  ],
			  [u_cx_c36_touch_fact, u_xx_reality_xx, u_emot],
			  
			  [ if,
			    
			    [ u_fl_c60,
			      [u_strength, u_emot],
			      u_xx_emotion_gc_threshold_xx
			    ],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 "Emotion ~A below threshold."),
				u_emot
			      ],
			      [u_cx_c36_retract, u_xx_reality_xx, u_emot],
			      [u_emotion_delete, u_emot]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_emotion_decay, classof, claz_function),
   set_opv(u_emotion_decay, compile_as, kw_function),
   set_opv(u_emotion_decay, function, f_u_emotion_decay),
   DefunResult=u_emotion_decay.
/*
:- side_effect(assert_lsp(u_emotion_decay,
			  wl:lambda_def(defun, u_emotion_decay, f_u_emotion_decay, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Emotion decay.")], [u_yloop, [u_yfor, u_emot, u_in, u_xx_emotions_xx], [u_ydo, [if, [not, [u_motivating_emotion_c63, u_emot]], [progn, [u_set_strength, u_emot, [u_fl_xx, u_xx_emotion_decay_factor_xx, [u_strength, u_emot]]], [u_cx_c36_touch_fact, u_xx_reality_xx, u_emot], [if, [u_fl_c60, [u_strength, u_emot], u_xx_emotion_gc_threshold_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Emotion ~A below threshold."), u_emot], [u_cx_c36_retract, u_xx_reality_xx, u_emot], [u_emotion_delete, u_emot]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_emotion_decay,
			  wl:arglist_info(u_emotion_decay, f_u_emotion_decay, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_emotion_decay,
			  wl:init_args(exact_only, f_u_emotion_decay))).
*/
/*
(defun emotion-delete (emot)
  (setq *emotions* (delq! emot *emotions*))
  *emotions*)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7820 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'emotion-delete',[emot],[setq,'*emotions*',['delq!',emot,'*emotions*']],'*emotions*'])
wl:lambda_def(defun, u_emotion_delete, f_u_emotion_delete, [u_emot], [[setq, u_xx_emotions_xx, [u_delq_c33, u_emot, u_xx_emotions_xx]], u_xx_emotions_xx]).
wl:arglist_info(u_emotion_delete, f_u_emotion_delete, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_emotion_delete).

/*

### Compiled:  `U::EMOTION-DELETE` 
*/
f_u_emotion_delete(Emot, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_emot, Emot)|Env],
	f_u_delq_c33(u_emot, u_xx_emotions_xx, Xx_emotions_xx),
	set_var(AEnv, u_xx_emotions_xx, Xx_emotions_xx),
	get_var(AEnv, u_xx_emotions_xx, Xx_emotions_xx_Get),
	Xx_emotions_xx_Get=FnResult.
:- set_opv(f_u_emotion_delete, classof, claz_function),
   set_opv(u_emotion_delete, compile_as, kw_function),
   set_opv(u_emotion_delete, function, f_u_emotion_delete),
   DefunResult=u_emotion_delete.
/*
:- side_effect(assert_lsp(u_emotion_delete,
			  wl:lambda_def(defun, u_emotion_delete, f_u_emotion_delete, [u_emot], [[setq, u_xx_emotions_xx, [u_delq_c33, u_emot, u_xx_emotions_xx]], u_xx_emotions_xx]))).
*/
/*
:- side_effect(assert_lsp(u_emotion_delete,
			  wl:arglist_info(u_emotion_delete, f_u_emotion_delete, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_emotion_delete,
			  wl:init_args(exact_only, f_u_emotion_delete))).
*/
/*
(defun emotion-add (emot)
  (if (not (memq? emot *emotions*))
      (setq *emotions* (cons emot *emotions*)))
  emot)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7908 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'emotion-add',[emot],[if,[not,['memq?',emot,'*emotions*']],[setq,'*emotions*',[cons,emot,'*emotions*']]],emot])
wl:lambda_def(defun, u_emotion_add, f_u_emotion_add, [u_emot], [[if, [not, [u_memq_c63, u_emot, u_xx_emotions_xx]], [setq, u_xx_emotions_xx, [cons, u_emot, u_xx_emotions_xx]]], u_emot]).
wl:arglist_info(u_emotion_add, f_u_emotion_add, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_emotion_add).

/*

### Compiled:  `U::EMOTION-ADD` 
*/
f_u_emotion_add(Emot, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_emot, Emot)|Env],
	f_u_memq_c63(u_emot, u_xx_emotions_xx, PredArgResult),
	(   PredArgResult==[]
	->  get_var(AEnv, u_emot, Emot_Get),
	    get_var(AEnv, u_xx_emotions_xx, Xx_emotions_xx_Get),
	    TrueResult=[Emot_Get|Xx_emotions_xx_Get],
	    set_var(AEnv, u_xx_emotions_xx, TrueResult),
	    _159437750=TrueResult
	;   _159437750=[]
	),
	get_var(AEnv, u_emot, Emot_Get11),
	Emot_Get11=FnResult.
:- set_opv(f_u_emotion_add, classof, claz_function),
   set_opv(u_emotion_add, compile_as, kw_function),
   set_opv(u_emotion_add, function, f_u_emotion_add),
   DefunResult=u_emotion_add.
/*
:- side_effect(assert_lsp(u_emotion_add,
			  wl:lambda_def(defun, u_emotion_add, f_u_emotion_add, [u_emot], [[if, [not, [u_memq_c63, u_emot, u_xx_emotions_xx]], [setq, u_xx_emotions_xx, [cons, u_emot, u_xx_emotions_xx]]], u_emot]))).
*/
/*
:- side_effect(assert_lsp(u_emotion_add,
			  wl:arglist_info(u_emotion_add, f_u_emotion_add, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_emotion_add,
			  wl:init_args(exact_only, f_u_emotion_add))).
*/
/*
(defun motivating-emotion? (emot)
  (any? (lambda (ob) (ty$instance? ob 'goal)) ; or (memq? ob *top-level-goals*)
        (get-dependees emot *reality* *me-belief-path*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8027 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'motivating-emotion?',[emot],['any?',[lambda,[ob],['ty$instance?',ob,[quote,goal]]],['get-dependees',emot,'*reality*','*me-belief-path*']]])
wl:lambda_def(defun, u_motivating_emotion_c63, f_u_motivating_emotion_c63, [u_emot], [[u_any_c63, [lambda, [u_ob], [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]], [u_get_dependees, u_emot, u_xx_reality_xx, u_xx_me_belief_path_xx]]]).
wl:arglist_info(u_motivating_emotion_c63, f_u_motivating_emotion_c63, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_motivating_emotion_c63).

/*

### Compiled:  `U::MOTIVATING-EMOTION?` 
*/
f_u_motivating_emotion_c63(Emot, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_emot, Emot)|Env],
	f_u_any_c63(
		    [ lambda,
		      [u_ob],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]
		    ],
		    
		    [ u_get_dependees,
		      u_emot,
		      u_xx_reality_xx,
		      u_xx_me_belief_path_xx
		    ],
		    Any_c63_Ret),
	Any_c63_Ret=FnResult.
:- set_opv(f_u_motivating_emotion_c63, classof, claz_function),
   set_opv(u_motivating_emotion_c63, compile_as, kw_function),
   set_opv(u_motivating_emotion_c63, function, f_u_motivating_emotion_c63),
   DefunResult=u_motivating_emotion_c63.
/*
:- side_effect(assert_lsp(u_motivating_emotion_c63,
			  wl:lambda_def(defun, u_motivating_emotion_c63, f_u_motivating_emotion_c63, [u_emot], [[u_any_c63, [lambda, [u_ob], [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]], [u_get_dependees, u_emot, u_xx_reality_xx, u_xx_me_belief_path_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_motivating_emotion_c63,
			  wl:arglist_info(u_motivating_emotion_c63, f_u_motivating_emotion_c63, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_motivating_emotion_c63,
			  wl:init_args(exact_only, f_u_motivating_emotion_c63))).
*/
/*
 or (memq? ob *top-level-goals*)
*/
/*
(setq *top-level-goals* nil)

;
; Find the top-level goals which are most highly motivated and are
; not halted, and if in performance mode, are not imaginary.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*top-level-goals*',[]])
:- set_var(AEnv, setq, u_xx_top_level_goals_xx, []).
/*
*/
/*
 Find the top-level goals which are most highly motivated and are
*/
/*
 not halted, and if in performance mode, are not imaginary.
*/
/*
*/
/*
(defun most-highly-motivated-goals ()
  (yloop (initial (highest-strength 0.0)
;                 (emotion nil)
                  (candidates nil))
         (yfor top-level-goal in *top-level-goals*)
         (ydo (if (and (eq? 'runable
                       (ob$get top-level-goal 'status))
                       (or (daydreaming-mode?)
                           (neq? 'imaginary
                                 (ob$get top-level-goal 'planning-type))))
                  (progn
;                  (setq emotion (ob$get top-level-goal 'emotion))
;                  (if (null? emotion)
;                      (error "No motivating emotion found for "(defun most-highly-motivated-goals ()\n  (yloop (initial (highest-strength 0.0)\n;                 (emotion nil)\n                  (candidates nil))\n         (yfor top-level-goal in *top-level-goals*)\n         (ydo (if (and (eq? 'runable\n                       (ob$get top-level-goal 'status))\n                       (or (daydreaming-mode?)\n                           (neq? 'imaginary\n                                 (ob$get top-level-goal 'planning-type))))\n                  (progn\n;                  (setq emotion (ob$get top-level-goal 'emotion))\n;                  (if (null? emotion)\n;                      (error \"No motivating emotion found for ~A\"\n;                             top-level-goal))\n                   (cond\n                    ((= (strength top-level-goal) highest-strength)\n                     (setq candidates (cons top-level-goal candidates)))\n                    ((> (strength top-level-goal) highest-strength)\n                     (setq highest-strength (strength top-level-goal))\n                     (setq candidates (list top-level-goal)))))))\n         (yresult candidates)))\n\n;\n; Debugging functions\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'most-highly-motivated-goals',[],[yloop,[initial,['highest-strength',0.0],[candidates,[]]],[yfor,'top-level-goal',in,'*top-level-goals*'],[ydo,[if,[and,['eq?',[quote,runable],['ob$get','top-level-goal',[quote,status]]],[or,['daydreaming-mode?'],['neq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]]]],[progn,[cond,[[=,[strength,'top-level-goal'],'highest-strength'],[setq,candidates,[cons,'top-level-goal',candidates]]],[[>,[strength,'top-level-goal'],'highest-strength'],[setq,'highest-strength',[strength,'top-level-goal']],[setq,candidates,[list,'top-level-goal']]]]]]],[yresult,candidates]]])
wl:lambda_def(defun, u_most_highly_motivated_goals, f_u_most_highly_motivated_goals, [], [[u_yloop, [u_initial, [u_highest_strength, 0.0], [u_candidates, []]], [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx], [u_ydo, [if, [and, [u_eq_c63, [quote, u_runable], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [or, [u_daydreaming_mode_c63], [u_neq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]]]], [progn, [cond, [[=, [u_strength, u_top_level_goal], u_highest_strength], [setq, u_candidates, [cons, u_top_level_goal, u_candidates]]], [[>, [u_strength, u_top_level_goal], u_highest_strength], [setq, u_highest_strength, [u_strength, u_top_level_goal]], [setq, u_candidates, [list, u_top_level_goal]]]]]]], [u_yresult, u_candidates]]]).
wl:arglist_info(u_most_highly_motivated_goals, f_u_most_highly_motivated_goals, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_most_highly_motivated_goals).

/*

### Compiled:  `U::MOST-HIGHLY-MOTIVATED-GOALS` 
*/
f_u_most_highly_motivated_goals(FnResult) :-
	nop(global_env(Env)),
	_161178472=Env,
	f_u_yloop(
		  [ [u_initial, [u_highest_strength, 0.0], [u_candidates, []]],
		    [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_eq_c63,
			    [quote, u_runable],
			    [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			  ],
			  
			  [ or,
			    [u_daydreaming_mode_c63],
			    
			    [ u_neq_c63,
			      [quote, u_imaginary],
			      
			      [ u_ob_c36_get,
				u_top_level_goal,
				[quote, u_planning_type]
			      ]
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ cond,
			    
			    [ 
			      [ (=),
				[u_strength, u_top_level_goal],
				u_highest_strength
			      ],
			      
			      [ setq,
				u_candidates,
				[cons, u_top_level_goal, u_candidates]
			      ]
			    ],
			    
			    [ 
			      [ (>),
				[u_strength, u_top_level_goal],
				u_highest_strength
			      ],
			      
			      [ setq,
				u_highest_strength,
				[u_strength, u_top_level_goal]
			      ],
			      [setq, u_candidates, [list, u_top_level_goal]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_candidates]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_most_highly_motivated_goals, classof, claz_function),
   set_opv(u_most_highly_motivated_goals, compile_as, kw_function),
   set_opv(u_most_highly_motivated_goals,
	   function,
	   f_u_most_highly_motivated_goals),
   DefunResult=u_most_highly_motivated_goals.
/*
:- side_effect(assert_lsp(u_most_highly_motivated_goals,
			  wl:lambda_def(defun, u_most_highly_motivated_goals, f_u_most_highly_motivated_goals, [], [[u_yloop, [u_initial, [u_highest_strength, 0.0], [u_candidates, []]], [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx], [u_ydo, [if, [and, [u_eq_c63, [quote, u_runable], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [or, [u_daydreaming_mode_c63], [u_neq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]]]], [progn, [cond, [[=, [u_strength, u_top_level_goal], u_highest_strength], [setq, u_candidates, [cons, u_top_level_goal, u_candidates]]], [[>, [u_strength, u_top_level_goal], u_highest_strength], [setq, u_highest_strength, [u_strength, u_top_level_goal]], [setq, u_candidates, [list, u_top_level_goal]]]]]]], [u_yresult, u_candidates]]]))).
*/
/*
:- side_effect(assert_lsp(u_most_highly_motivated_goals,
			  wl:arglist_info(u_most_highly_motivated_goals, f_u_most_highly_motivated_goals, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_most_highly_motivated_goals,
			  wl:init_args(exact_only, f_u_most_highly_motivated_goals))).
*/
/*
                 (emotion nil)
*/
/*
                  (setq emotion (ob$get top-level-goal 'emotion))
*/
/*
                  (if (null? emotion)
*/
/*
                      (error "No motivating emotion found for "                      (error \"No motivating emotion found for ~A\"".
*/
/*
                             top-level-goal))
*/
/*
*/
/*
 Debugging functions
*/
/*
*/
/*
(defun print-tasks ()
  (yloop
   (yfor c in *top-level-goals*)
   (ydo (ndbg-roman-nl *gate-dbg* task ""(defun print-tasks ()\n  (yloop\n   (yfor c in *top-level-goals*)\n   (ydo (ndbg-roman-nl *gate-dbg* task \"~A concern ~A motiv ~A status ~A\"\n                       (if (eq? 'imaginary (ob$get c 'planning-type))\n                           \"Daydreaming goal\"\n                           \"Personal goal\")\n                       (tlg->string c)\n                       (strength c)\n                       (ob$get c 'status)))))\n\n;\n; Control algorithm for a particular top-level-goal:\n; 1) Run one step of the planner on the next context.\n; 2) If the top-level goal succeeded, terminate planning for this\n;    top-level goal.\n; 3) Otherwise if running the planner produced no sprouts, then\n;    attempt to backtrack.\n; 4) Otherwise set the next context to run to be one of the sprouts,\n;    selected at random.\n;\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'print-tasks',[],[yloop,[yfor,c,in,'*top-level-goals*'],[ydo,['ndbg-roman-nl','*gate-dbg*',task,'$STRING'("~A concern ~A motiv ~A status ~A"),[if,['eq?',[quote,imaginary],['ob$get',c,[quote,'planning-type']]],'$STRING'("Daydreaming goal"),'$STRING'("Personal goal")],['tlg->string',c],[strength,c],['ob$get',c,[quote,status]]]]]])
wl:lambda_def(defun, u_print_tasks, f_u_print_tasks, [], [[u_yloop, [u_yfor, u_c, u_in, u_xx_top_level_goals_xx], [u_ydo, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_task, '$ARRAY'([*], claz_base_character, "~A concern ~A motiv ~A status ~A"), [if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_c, [quote, u_planning_type]]], '$ARRAY'([*], claz_base_character, "Daydreaming goal"), '$ARRAY'([*], claz_base_character, "Personal goal")], [u_tlg_c62_string, u_c], [u_strength, u_c], [u_ob_c36_get, u_c, [quote, u_status]]]]]]).
wl:arglist_info(u_print_tasks, f_u_print_tasks, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_print_tasks).

/*

### Compiled:  `U::PRINT-TASKS` 
*/
f_u_print_tasks(FnResult) :-
	nop(global_env(Env)),
	_162478472=Env,
	f_u_yloop(
		  [ [u_yfor, u_c, u_in, u_xx_top_level_goals_xx],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_task,
			'$ARRAY'([*],
				 claz_base_character,
				 "~A concern ~A motiv ~A status ~A"),
			
			[ if,
			  
			  [ u_eq_c63,
			    [quote, u_imaginary],
			    [u_ob_c36_get, u_c, [quote, u_planning_type]]
			  ],
			  '$ARRAY'([*], claz_base_character, "Daydreaming goal"),
			  '$ARRAY'([*], claz_base_character, "Personal goal")
			],
			[u_tlg_c62_string, u_c],
			[u_strength, u_c],
			[u_ob_c36_get, u_c, [quote, u_status]]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_print_tasks, classof, claz_function),
   set_opv(u_print_tasks, compile_as, kw_function),
   set_opv(u_print_tasks, function, f_u_print_tasks),
   DefunResult=u_print_tasks.
/*
:- side_effect(assert_lsp(u_print_tasks,
			  wl:lambda_def(defun, u_print_tasks, f_u_print_tasks, [], [[u_yloop, [u_yfor, u_c, u_in, u_xx_top_level_goals_xx], [u_ydo, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_task, '$ARRAY'([*], claz_base_character, "~A concern ~A motiv ~A status ~A"), [if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_c, [quote, u_planning_type]]], '$ARRAY'([*], claz_base_character, "Daydreaming goal"), '$ARRAY'([*], claz_base_character, "Personal goal")], [u_tlg_c62_string, u_c], [u_strength, u_c], [u_ob_c36_get, u_c, [quote, u_status]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_print_tasks,
			  wl:arglist_info(u_print_tasks, f_u_print_tasks, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_print_tasks,
			  wl:init_args(exact_only, f_u_print_tasks))).
*/
/*
*/
/*
 Control algorithm for a particular top-level-goal:
*/
/*
 1) Run one step of the planner on the next context.
*/
/*
 2) If the top-level goal succeeded, terminate planning for this
*/
/*
    top-level goal.
*/
/*
 3) Otherwise if running the planner produced no sprouts, then
*/
/*
    attempt to backtrack.
*/
/*
 4) Otherwise set the next context to run to be one of the sprouts,
*/
/*
    selected at random.
*/
/*
*/
/*
(setq *top-level-goal* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10299 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*top-level-goal*',[]])
:- set_var(AEnv, setq, u_xx_top_level_goal_xx, []).
/*
(defun daydreamer-control1 (top-level-goal)
  (let ((next-context (get-next-context top-level-goal))
        (sprouts nil)
        (tlg-switch? (neq? top-level-goal *top-level-goal*))
        (succeeded-goal nil))
       (if tlg-switch? (gen-new-paragraph))
       (setq *top-level-goal* top-level-goal)
       (ndbg-roman-nl *gate-dbg* rule-long
                      "Running control algorithm for "(defun daydreamer-control1 (top-level-goal)\n  (let ((next-context (get-next-context top-level-goal))\n        (sprouts nil)\n        (tlg-switch? (neq? top-level-goal *top-level-goal*))\n        (succeeded-goal nil))\n       (if tlg-switch? (gen-new-paragraph))\n       (setq *top-level-goal* top-level-goal)\n       (ndbg-roman-nl *gate-dbg* rule-long\n                      \"Running control algorithm for ~A in ~A\"\n                      top-level-goal next-context)\n       (if (and (number? (ob$get next-context 'timeout))\n                (<= (ob$get next-context 'timeout) 0))\n           (progn\n            (ndbg-roman *gate-dbg* rule \"Timeout\")\n            (ndbg-roman *gate-dbg* rule \" on ~A\" next-context)\n            (ndbg-newline *gate-dbg* rule)\n            (backtrack-top-level-goal top-level-goal next-context))\n           (progn\n            (run-rules top-level-goal next-context tlg-switch?)\n            (if (setq succeeded-goal (find-top-level-goal-outcome?\n                                      top-level-goal next-context\n                                      *succeeded-goal-ob*))\n                ; Todo: eventually we may wish to run scenario generator\n                ; past top-level goal success.\n                (terminate-top-level-goal top-level-goal succeeded-goal\n                                          next-context)\n           (progn\n            (if (or (eq? 'runable (ob$get top-level-goal 'status))\n                    (eq? 'fired-halt (ob$get top-level-goal 'status)))\n                (progn\n                 (if (eq? 'fired-halt (ob$get top-level-goal 'status))\n                     (change-tlg-status top-level-goal 'halted))\n                 (setq sprouts (prune-possibilities (cx$children next-context)))\n                 (if (null? sprouts)\n                     ; should include case of top-level goal failure\n                     (backtrack-top-level-goal top-level-goal next-context)\n                     (set-next-context top-level-goal\n                                       (car sprouts)))))))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10328 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'daydreamer-control1',['top-level-goal'],[let,[['next-context',['get-next-context','top-level-goal']],[sprouts,[]],['tlg-switch?',['neq?','top-level-goal','*top-level-goal*']],['succeeded-goal',[]]],[if,'tlg-switch?',['gen-new-paragraph']],[setq,'*top-level-goal*','top-level-goal'],['ndbg-roman-nl','*gate-dbg*','rule-long','$STRING'("Running control algorithm for ~A in ~A"),'top-level-goal','next-context'],[if,[and,['number?',['ob$get','next-context',[quote,timeout]]],[<=,['ob$get','next-context',[quote,timeout]],0]],[progn,['ndbg-roman','*gate-dbg*',rule,'$STRING'("Timeout")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" on ~A"),'next-context'],['ndbg-newline','*gate-dbg*',rule],['backtrack-top-level-goal','top-level-goal','next-context']],[progn,['run-rules','top-level-goal','next-context','tlg-switch?'],[if,[setq,'succeeded-goal',['find-top-level-goal-outcome?','top-level-goal','next-context','*succeeded-goal-ob*']],['terminate-top-level-goal','top-level-goal','succeeded-goal','next-context'],[progn,[if,[or,['eq?',[quote,runable],['ob$get','top-level-goal',[quote,status]]],['eq?',[quote,'fired-halt'],['ob$get','top-level-goal',[quote,status]]]],[progn,[if,['eq?',[quote,'fired-halt'],['ob$get','top-level-goal',[quote,status]]],['change-tlg-status','top-level-goal',[quote,halted]]],[setq,sprouts,['prune-possibilities',['cx$children','next-context']]],[if,['null?',sprouts],['backtrack-top-level-goal','top-level-goal','next-context'],['set-next-context','top-level-goal',[car,sprouts]]]]]]]]]]])
wl:lambda_def(defun, u_daydreamer_control1, f_u_daydreamer_control1, [u_top_level_goal], [[let, [[u_next_context, [u_get_next_context, u_top_level_goal]], [u_sprouts, []], [u_tlg_switch_c63, [u_neq_c63, u_top_level_goal, u_xx_top_level_goal_xx]], [u_succeeded_goal, []]], [if, u_tlg_switch_c63, [u_gen_new_paragraph]], [setq, u_xx_top_level_goal_xx, u_top_level_goal], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Running control algorithm for ~A in ~A"), u_top_level_goal, u_next_context], [if, [and, [u_number_c63, [u_ob_c36_get, u_next_context, [quote, ext_timeout]]], [<=, [u_ob_c36_get, u_next_context, [quote, ext_timeout]], 0]], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Timeout")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " on ~A"), u_next_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_backtrack_top_level_goal, u_top_level_goal, u_next_context]], [progn, [u_run_rules, u_top_level_goal, u_next_context, u_tlg_switch_c63], [if, [setq, u_succeeded_goal, [u_find_top_level_goal_outcome_c63, u_top_level_goal, u_next_context, u_xx_succeeded_goal_ob_xx]], [u_terminate_top_level_goal, u_top_level_goal, u_succeeded_goal, u_next_context], [progn, [if, [or, [u_eq_c63, [quote, u_runable], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [u_eq_c63, [quote, u_fired_halt], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]]], [progn, [if, [u_eq_c63, [quote, u_fired_halt], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [u_change_tlg_status, u_top_level_goal, [quote, u_halted]]], [setq, u_sprouts, [u_prune_possibilities, [u_cx_c36_children, u_next_context]]], [if, [u_null_c63, u_sprouts], [u_backtrack_top_level_goal, u_top_level_goal, u_next_context], [u_set_next_context, u_top_level_goal, [car, u_sprouts]]]]]]]]]]]).
wl:arglist_info(u_daydreamer_control1, f_u_daydreamer_control1, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_daydreamer_control1).

/*

### Compiled:  `U::DAYDREAMER-CONTROL1` 
*/
f_u_daydreamer_control1(Top_level_goal, TrueResult51) :-
	nop(global_env(Env)),
	Env58=[bv(u_top_level_goal, Top_level_goal)|Env],
	get_var(Env58, u_top_level_goal, Top_level_goal_Get),
	f_u_get_next_context(Top_level_goal_Get, Next_context_Init),
	f_u_neq_c63(u_top_level_goal,
		    u_xx_top_level_goal_xx,
		    Tlg_switch_c63_Init),
	LEnv=[bv(u_next_context, Next_context_Init), bv(u_sprouts, []), bv(u_tlg_switch_c63, Tlg_switch_c63_Init), bv(u_succeeded_goal, [])|Env58],
	get_var(LEnv, u_tlg_switch_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_gen_new_paragraph(TrueResult),
	    _164129738=TrueResult
	;   _164129738=[]
	),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get15),
	set_var(LEnv, u_xx_top_level_goal_xx, Top_level_goal_Get15),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Running control algorithm for ~A in ~A"),
			    u_top_level_goal,
			    u_next_context
			  ],
			  Roman_nl_Ret),
	f_u_number_c63([u_ob_c36_get, u_next_context, [quote, ext_timeout]],
		       IFTEST18),
	(   IFTEST18\==[]
	->  get_var(LEnv, u_next_context, Next_context_Get),
	    f_u_ob_c36_get(Next_context_Get, ext_timeout, Ext_timeout),
	    <=(Ext_timeout, 0, TrueResult21),
	    IFTEST16=TrueResult21
	;   IFTEST16=[]
	),
	(   IFTEST16\==[]
	->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
			   u_rule,
			   ['$ARRAY'([*], claz_base_character, "Timeout")],
			   Ndbg_roman_Ret),
	    f_u_ndbg_roman(u_xx_gate_dbg_xx,
			   u_rule,
			   
			   [ '$ARRAY'([*], claz_base_character, " on ~A"),
			     u_next_context
			   ],
			   Ndbg_roman_Ret66),
	    f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	    get_var(LEnv, u_next_context, Next_context_Get23),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get22),
	    f_u_backtrack_top_level_goal(Top_level_goal_Get22,
					 Next_context_Get23,
					 TrueResult54),
	    TrueResult51=TrueResult54
	;   get_var(LEnv, u_next_context, Next_context_Get25),
	    get_var(LEnv, u_tlg_switch_c63, Tlg_switch_c63_Get26),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get24),
	    f_u_run_rules(Top_level_goal_Get24,
			  Next_context_Get25,
			  Tlg_switch_c63_Get26,
			  Run_rules_Ret),
	    get_var(LEnv, u_next_context, Next_context_Get30),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get29),
	    get_var(LEnv,
		    u_xx_succeeded_goal_ob_xx,
		    Xx_succeeded_goal_ob_xx_Get),
	    f_u_find_top_level_goal_outcome_c63(Top_level_goal_Get29,
						Next_context_Get30,
						Xx_succeeded_goal_ob_xx_Get,
						IFTEST27),
	    set_var(LEnv, u_succeeded_goal, IFTEST27),
	    (   IFTEST27\==[]
	    ->  get_var(LEnv, u_next_context, Next_context_Get34),
		get_var(LEnv, u_succeeded_goal, Succeeded_goal_Get),
		get_var(LEnv, u_top_level_goal, Top_level_goal_Get32),
		f_u_terminate_top_level_goal(Top_level_goal_Get32,
					     Succeeded_goal_Get,
					     Next_context_Get34,
					     TrueResult52),
		TrueResult51=TrueResult52
	    ;   (   f_u_eq_c63([quote, u_runable],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST35=FORM1_Res
		->  true
		;   f_u_eq_c63([quote, u_fired_halt],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       Eq_c63_Ret),
		    IFTEST35=Eq_c63_Ret
		),
		(   IFTEST35\==[]
		->  f_u_eq_c63([quote, u_fired_halt],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       IFTEST38),
		    (   IFTEST38\==[]
		    ->  get_var(LEnv, u_top_level_goal, Top_level_goal_Get40),
			f_u_change_tlg_status(Top_level_goal_Get40,
					      u_halted,
					      TrueResult41),
			_164265808=TrueResult41
		    ;   _164265808=[]
		    ),
		    get_var(LEnv, u_next_context, Next_context_Get42),
		    f_u_cx_c36_children(Next_context_Get42,
					Prune_possibilities_Param),
		    f_u_prune_possibilities(Prune_possibilities_Param, Sprouts),
		    set_var(LEnv, u_sprouts, Sprouts),
		    f_u_null_c63(u_sprouts, IFTEST43),
		    (   IFTEST43\==[]
		    ->  get_var(LEnv, u_next_context, Next_context_Get46),
			get_var(LEnv, u_top_level_goal, Top_level_goal_Get45),
			f_u_backtrack_top_level_goal(Top_level_goal_Get45,
						     Next_context_Get46,
						     TrueResult49),
			TrueResult51=TrueResult49
		    ;   get_var(LEnv, u_sprouts, Sprouts_Get),
			get_var(LEnv, u_top_level_goal, Top_level_goal_Get47),
			cl_car(Sprouts_Get, Car_Ret),
			f_u_set_next_context(Top_level_goal_Get47,
					     Car_Ret,
					     ElseResult),
			TrueResult51=ElseResult
		    )
		;   TrueResult51=[]
		)
	    )
	).
:- set_opv(f_u_daydreamer_control1, classof, claz_function),
   set_opv(u_daydreamer_control1, compile_as, kw_function),
   set_opv(u_daydreamer_control1, function, f_u_daydreamer_control1),
   DefunResult=u_daydreamer_control1.
/*
:- side_effect(assert_lsp(u_daydreamer_control1,
			  wl:lambda_def(defun, u_daydreamer_control1, f_u_daydreamer_control1, [u_top_level_goal], [[let, [[u_next_context, [u_get_next_context, u_top_level_goal]], [u_sprouts, []], [u_tlg_switch_c63, [u_neq_c63, u_top_level_goal, u_xx_top_level_goal_xx]], [u_succeeded_goal, []]], [if, u_tlg_switch_c63, [u_gen_new_paragraph]], [setq, u_xx_top_level_goal_xx, u_top_level_goal], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Running control algorithm for ~A in ~A"), u_top_level_goal, u_next_context], [if, [and, [u_number_c63, [u_ob_c36_get, u_next_context, [quote, ext_timeout]]], [<=, [u_ob_c36_get, u_next_context, [quote, ext_timeout]], 0]], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Timeout")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " on ~A"), u_next_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_backtrack_top_level_goal, u_top_level_goal, u_next_context]], [progn, [u_run_rules, u_top_level_goal, u_next_context, u_tlg_switch_c63], [if, [setq, u_succeeded_goal, [u_find_top_level_goal_outcome_c63, u_top_level_goal, u_next_context, u_xx_succeeded_goal_ob_xx]], [u_terminate_top_level_goal, u_top_level_goal, u_succeeded_goal, u_next_context], [progn, [if, [or, [u_eq_c63, [quote, u_runable], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [u_eq_c63, [quote, u_fired_halt], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]]], [progn, [if, [u_eq_c63, [quote, u_fired_halt], [u_ob_c36_get, u_top_level_goal, [quote, u_status]]], [u_change_tlg_status, u_top_level_goal, [quote, u_halted]]], [setq, u_sprouts, [u_prune_possibilities, [u_cx_c36_children, u_next_context]]], [if, [u_null_c63, u_sprouts], [u_backtrack_top_level_goal, u_top_level_goal, u_next_context], [u_set_next_context, u_top_level_goal, [car, u_sprouts]]]]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_control1,
			  wl:arglist_info(u_daydreamer_control1, f_u_daydreamer_control1, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_daydreamer_control1,
			  wl:init_args(exact_only, f_u_daydreamer_control1))).
*/
/*
 Todo: eventually we may wish to run scenario generator
*/
/*
 past top-level goal success.
*/
/*
 should include case of top-level goal failure
*/
/*
(defun change-tlg-status (tlg status)
  (ndbg-roman *gate-dbg* rule
              "Change status of "(defun change-tlg-status (tlg status)\n  (ndbg-roman *gate-dbg* rule\n              \"Change status of ~A to ~A\"\n              (tlg->string tlg)\n; Can't print below because it might be FIRED-HALT, a weird state.\n;              (ob$get tlg 'status)\n              status)\n  (ob$set tlg 'status status))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12363 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'change-tlg-status',[tlg,status],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Change status of ~A to ~A"),['tlg->string',tlg],status],['ob$set',tlg,[quote,status],status]])
wl:lambda_def(defun, u_change_tlg_status, f_u_change_tlg_status, [u_tlg, u_status], [[u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Change status of ~A to ~A"), [u_tlg_c62_string, u_tlg], u_status], [u_ob_c36_set, u_tlg, [quote, u_status], u_status]]).
wl:arglist_info(u_change_tlg_status, f_u_change_tlg_status, [u_tlg, u_status], arginfo{all:[u_tlg, u_status], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_tlg, u_status], opt:0, req:[u_tlg, u_status], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_change_tlg_status).

/*

### Compiled:  `U::CHANGE-TLG-STATUS` 
*/
f_u_change_tlg_status(Tlg, Status, FnResult) :-
	nop(global_env(Env)),
	Env8=[bv(u_tlg, Tlg), bv(u_status, Status)|Env],
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  "Change status of ~A to ~A"),
			 [u_tlg_c62_string, u_tlg],
			 u_status
		       ],
		       Ndbg_roman_Ret),
	get_var(Env8, u_status, Status_Get),
	get_var(Env8, u_tlg, Tlg_Get),
	f_u_ob_c36_set(Tlg_Get, u_status, Status_Get, C36_set_Ret),
	C36_set_Ret=FnResult.
:- set_opv(f_u_change_tlg_status, classof, claz_function),
   set_opv(u_change_tlg_status, compile_as, kw_function),
   set_opv(u_change_tlg_status, function, f_u_change_tlg_status),
   DefunResult=u_change_tlg_status.
/*
:- side_effect(assert_lsp(u_change_tlg_status,
			  wl:lambda_def(defun, u_change_tlg_status, f_u_change_tlg_status, [u_tlg, u_status], [[u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Change status of ~A to ~A"), [u_tlg_c62_string, u_tlg], u_status], [u_ob_c36_set, u_tlg, [quote, u_status], u_status]]))).
*/
/*
:- side_effect(assert_lsp(u_change_tlg_status,
			  wl:arglist_info(u_change_tlg_status, f_u_change_tlg_status, [u_tlg, u_status], arginfo{all:[u_tlg, u_status], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_tlg, u_status], opt:0, req:[u_tlg, u_status], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_change_tlg_status,
			  wl:init_args(exact_only, f_u_change_tlg_status))).
*/
/*
 Can't print below because it might be FIRED-HALT, a weird state.
*/
/*
              (ob$get tlg 'status)
*/
/*
(defun get-next-context (top-level-goal)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$get top-level-goal 'next-context)
      *reality-lookahead*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12662 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-next-context',['top-level-goal'],[if,['eq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]],['ob$get','top-level-goal',[quote,'next-context']],'*reality-lookahead*']])
wl:lambda_def(defun, u_get_next_context, f_u_get_next_context, [u_top_level_goal], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_get, u_top_level_goal, [quote, u_next_context]], u_xx_reality_lookahead_xx]]).
wl:arglist_info(u_get_next_context, f_u_get_next_context, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_next_context).

/*

### Compiled:  `U::GET-NEXT-CONTEXT` 
*/
f_u_get_next_context(Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  get_var(Env12, u_top_level_goal, Top_level_goal_Get),
	    f_u_ob_c36_get(Top_level_goal_Get, u_next_context, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env12,
		    u_xx_reality_lookahead_xx,
		    Xx_reality_lookahead_xx_Get),
	    FnResult=Xx_reality_lookahead_xx_Get
	).
:- set_opv(f_u_get_next_context, classof, claz_function),
   set_opv(u_get_next_context, compile_as, kw_function),
   set_opv(u_get_next_context, function, f_u_get_next_context),
   DefunResult=u_get_next_context.
/*
:- side_effect(assert_lsp(u_get_next_context,
			  wl:lambda_def(defun, u_get_next_context, f_u_get_next_context, [u_top_level_goal], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_get, u_top_level_goal, [quote, u_next_context]], u_xx_reality_lookahead_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_get_next_context,
			  wl:arglist_info(u_get_next_context, f_u_get_next_context, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_next_context,
			  wl:init_args(exact_only, f_u_get_next_context))).
*/
/*
(defun set-next-context (top-level-goal next-context)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$set top-level-goal 'next-context next-context)
      (setq *reality-lookahead* next-context)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12838 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'set-next-context',['top-level-goal','next-context'],[if,['eq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]],['ob$set','top-level-goal',[quote,'next-context'],'next-context'],[setq,'*reality-lookahead*','next-context']]])
wl:lambda_def(defun, u_set_next_context, f_u_set_next_context, [u_top_level_goal, u_next_context], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_set, u_top_level_goal, [quote, u_next_context], u_next_context], [setq, u_xx_reality_lookahead_xx, u_next_context]]]).
wl:arglist_info(u_set_next_context, f_u_set_next_context, [u_top_level_goal, u_next_context], arginfo{all:[u_top_level_goal, u_next_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_next_context], opt:0, req:[u_top_level_goal, u_next_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_next_context).

/*

### Compiled:  `U::SET-NEXT-CONTEXT` 
*/
f_u_set_next_context(Top_level_goal, Next_context, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_top_level_goal, Top_level_goal), bv(u_next_context, Next_context)|Env],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_next_context, Next_context_Get),
	    get_var(AEnv, u_top_level_goal, Top_level_goal_Get),
	    f_u_ob_c36_set(Top_level_goal_Get,
			   u_next_context,
			   Next_context_Get,
			   TrueResult),
	    FnResult=TrueResult
	;   get_var(AEnv, u_next_context, Next_context_Get9),
	    set_var(AEnv, u_xx_reality_lookahead_xx, Next_context_Get9),
	    FnResult=Next_context_Get9
	).
:- set_opv(f_u_set_next_context, classof, claz_function),
   set_opv(u_set_next_context, compile_as, kw_function),
   set_opv(u_set_next_context, function, f_u_set_next_context),
   DefunResult=u_set_next_context.
/*
:- side_effect(assert_lsp(u_set_next_context,
			  wl:lambda_def(defun, u_set_next_context, f_u_set_next_context, [u_top_level_goal, u_next_context], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_set, u_top_level_goal, [quote, u_next_context], u_next_context], [setq, u_xx_reality_lookahead_xx, u_next_context]]]))).
*/
/*
:- side_effect(assert_lsp(u_set_next_context,
			  wl:arglist_info(u_set_next_context, f_u_set_next_context, [u_top_level_goal, u_next_context], arginfo{all:[u_top_level_goal, u_next_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_next_context], opt:0, req:[u_top_level_goal, u_next_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_next_context,
			  wl:init_args(exact_only, f_u_set_next_context))).
*/
/*
(defun get-backtrack-wall (top-level-goal)
  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))
      (ob$get top-level-goal 'backtrack-wall)
      *reality*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13060 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-backtrack-wall',['top-level-goal'],[if,['eq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]],['ob$get','top-level-goal',[quote,'backtrack-wall']],'*reality*']])
wl:lambda_def(defun, u_get_backtrack_wall, f_u_get_backtrack_wall, [u_top_level_goal], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_get, u_top_level_goal, [quote, u_backtrack_wall]], u_xx_reality_xx]]).
wl:arglist_info(u_get_backtrack_wall, f_u_get_backtrack_wall, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_backtrack_wall).

/*

### Compiled:  `U::GET-BACKTRACK-WALL` 
*/
f_u_get_backtrack_wall(Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  get_var(Env12, u_top_level_goal, Top_level_goal_Get),
	    f_u_ob_c36_get(Top_level_goal_Get, u_backtrack_wall, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env12, u_xx_reality_xx, Xx_reality_xx_Get),
	    FnResult=Xx_reality_xx_Get
	).
:- set_opv(f_u_get_backtrack_wall, classof, claz_function),
   set_opv(u_get_backtrack_wall, compile_as, kw_function),
   set_opv(u_get_backtrack_wall, function, f_u_get_backtrack_wall),
   DefunResult=u_get_backtrack_wall.
/*
:- side_effect(assert_lsp(u_get_backtrack_wall,
			  wl:lambda_def(defun, u_get_backtrack_wall, f_u_get_backtrack_wall, [u_top_level_goal], [[if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ob_c36_get, u_top_level_goal, [quote, u_backtrack_wall]], u_xx_reality_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_get_backtrack_wall,
			  wl:arglist_info(u_get_backtrack_wall, f_u_get_backtrack_wall, [u_top_level_goal], arginfo{all:[u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal], opt:0, req:[u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_backtrack_wall,
			  wl:init_args(exact_only, f_u_get_backtrack_wall))).
*/
/*
(defun find-top-level-goal-outcome? (top-level-goal context goal-type)
  (yloop (initial (found? nil))
         (yfor elem in (cx$get-all-ty context goal-type))
         (yuntil found?)
         (ydo (if (eq? top-level-goal elem) ; was (ob$get elem 'active-goal)
                  (setq found? elem)))
         (yresult found?)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13230 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'find-top-level-goal-outcome?',['top-level-goal',context,'goal-type'],[yloop,[initial,['found?',[]]],[yfor,elem,in,['cx$get-all-ty',context,'goal-type']],[yuntil,'found?'],[ydo,[if,['eq?','top-level-goal',elem],[setq,'found?',elem]]],[yresult,'found?']]])
wl:lambda_def(defun, u_find_top_level_goal_outcome_c63, f_u_find_top_level_goal_outcome_c63, [u_top_level_goal, u_context, u_goal_type], [[u_yloop, [u_initial, [u_found_c63, []]], [u_yfor, u_elem, u_in, [u_cx_c36_get_all_ty, u_context, u_goal_type]], [u_yuntil, u_found_c63], [u_ydo, [if, [u_eq_c63, u_top_level_goal, u_elem], [setq, u_found_c63, u_elem]]], [u_yresult, u_found_c63]]]).
wl:arglist_info(u_find_top_level_goal_outcome_c63, f_u_find_top_level_goal_outcome_c63, [u_top_level_goal, u_context, u_goal_type], arginfo{all:[u_top_level_goal, u_context, u_goal_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_context, u_goal_type], opt:0, req:[u_top_level_goal, u_context, u_goal_type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_find_top_level_goal_outcome_c63).

/*

### Compiled:  `U::FIND-TOP-LEVEL-GOAL-OUTCOME?` 
*/
f_u_find_top_level_goal_outcome_c63(Top_level_goal, Context, Goal_type, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_top_level_goal, Top_level_goal), bv(u_context, Context), bv(u_goal_type, Goal_type)|Env],
	f_u_yloop(
		  [ [u_initial, [u_found_c63, []]],
		    
		    [ u_yfor,
		      u_elem,
		      u_in,
		      [u_cx_c36_get_all_ty, u_context, u_goal_type]
		    ],
		    [u_yuntil, u_found_c63],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_top_level_goal, u_elem],
			[setq, u_found_c63, u_elem]
		      ]
		    ],
		    [u_yresult, u_found_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_find_top_level_goal_outcome_c63, classof, claz_function),
   set_opv(u_find_top_level_goal_outcome_c63, compile_as, kw_function),
   set_opv(u_find_top_level_goal_outcome_c63,
	   function,
	   f_u_find_top_level_goal_outcome_c63),
   DefunResult=u_find_top_level_goal_outcome_c63.
/*
:- side_effect(assert_lsp(u_find_top_level_goal_outcome_c63,
			  wl:lambda_def(defun, u_find_top_level_goal_outcome_c63, f_u_find_top_level_goal_outcome_c63, [u_top_level_goal, u_context, u_goal_type], [[u_yloop, [u_initial, [u_found_c63, []]], [u_yfor, u_elem, u_in, [u_cx_c36_get_all_ty, u_context, u_goal_type]], [u_yuntil, u_found_c63], [u_ydo, [if, [u_eq_c63, u_top_level_goal, u_elem], [setq, u_found_c63, u_elem]]], [u_yresult, u_found_c63]]]))).
*/
/*
:- side_effect(assert_lsp(u_find_top_level_goal_outcome_c63,
			  wl:arglist_info(u_find_top_level_goal_outcome_c63, f_u_find_top_level_goal_outcome_c63, [u_top_level_goal, u_context, u_goal_type], arginfo{all:[u_top_level_goal, u_context, u_goal_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_context, u_goal_type], opt:0, req:[u_top_level_goal, u_context, u_goal_type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_find_top_level_goal_outcome_c63,
			  wl:init_args(exact_only, f_u_find_top_level_goal_outcome_c63))).
*/
/*
 was (ob$get elem 'active-goal)
*/
/*
(defun terminate-top-level-goal (top-level-goal resolved-goal
                                 resolution-context)
  (reality-stabilize) ; necessary?
  (ndbg-roman *gate-dbg* rule "Terminating planning for top-level goal")
  (ndbg-roman *gate-dbg* rule " "(defun terminate-top-level-goal (top-level-goal resolved-goal\n                                 resolution-context)\n  (reality-stabilize) ; necessary?\n  (ndbg-roman *gate-dbg* rule \"Terminating planning for top-level goal\")\n  (ndbg-roman *gate-dbg* rule \" ~A\" top-level-goal)\n  (ndbg-newline *gate-dbg* rule)\n  (task-print-plans top-level-goal)\n  (ob$set resolved-goal 'termination-context resolution-context)\n  ; Stop any future planning on this top-level goal\n  (setq *top-level-goals* (delq! top-level-goal *top-level-goals*))\n  ; Update to trace the list of tasks\n  (print-tasks)\n  ; Remove emotional motivators for active goal (which got carried\n  ; over to resolved goal)\n  (remove-motivating-emotions *reality* resolved-goal)\n  ; Invoke general emotional response\n  (if (or (not (dd-goal? resolved-goal))\n          (ty$instance? (ob$get resolved-goal 'obj) 'revenge))\n      (progn\n       (emotional-response resolved-goal nil\n                           (strength (ob$get resolved-goal 'obj))\n                           resolution-context)\n;       (gen-overall-emot-state) Now done in dd_gen\n))\n  ; Store the successful planning episode in episodic memory\n  (if (ty$instance? resolved-goal 'succeeded-goal)\n      (episode-store-top-goal resolved-goal resolution-context))\n  ; If top-level goal was imaginary planning, assert final goal status\n  ; (and objective if goal success) into reality context.\n  (if (eq? 'imaginary (ob$get top-level-goal 'planning-type))\n      (no-gen\n       (cx$assert *reality* resolved-goal)\n       (if (ty$instance? resolved-goal 'succeeded-goal)\n           (cx$assert *reality* (ob$get resolved-goal 'obj)))))\n  ; If top-level goal was real planning and goal failure, we better assert\n  ; the failure into the reality context in case it isn't already there.\n  (if (and (eq? 'real (ob$get top-level-goal 'planning-type))\n           (ty$instance? resolved-goal 'failed-goal))\n      (no-gen (cx$assert *reality* resolved-goal)))\n  (ndbg-roman-nl *gate-dbg* rule-xtra \"About to sprout ~A\" *reality*)\n  (setq *reality* (cx$sprout *reality*))\n  (ndbg-roman-nl *gate-dbg* rule-xtra \"Back from sprouting\")\n  (setq *reality-lookahead* *reality*)\n  (clear-subgoals resolved-goal *reality* *me-belief-path*)\n; (gc-plans1 *reality* (list top-level-goal))\n; If we use gc-plans1 here, we must modify it so it does not retract\n; side-effect goal outcomes.\n)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'terminate-top-level-goal',['top-level-goal','resolved-goal','resolution-context'],['reality-stabilize'],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Terminating planning for top-level goal")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A"),'top-level-goal'],['ndbg-newline','*gate-dbg*',rule],['task-print-plans','top-level-goal'],['ob$set','resolved-goal',[quote,'termination-context'],'resolution-context'],[setq,'*top-level-goals*',['delq!','top-level-goal','*top-level-goals*']],['print-tasks'],['remove-motivating-emotions','*reality*','resolved-goal'],[if,[or,[not,['dd-goal?','resolved-goal']],['ty$instance?',['ob$get','resolved-goal',[quote,obj]],[quote,revenge]]],[progn,['emotional-response','resolved-goal',[],[strength,['ob$get','resolved-goal',[quote,obj]]],'resolution-context']]],[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['episode-store-top-goal','resolved-goal','resolution-context']],[if,['eq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]],['no-gen',['cx$assert','*reality*','resolved-goal'],[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['cx$assert','*reality*',['ob$get','resolved-goal',[quote,obj]]]]]],[if,[and,['eq?',[quote,real],['ob$get','top-level-goal',[quote,'planning-type']]],['ty$instance?','resolved-goal',[quote,'failed-goal']]],['no-gen',['cx$assert','*reality*','resolved-goal']]],['ndbg-roman-nl','*gate-dbg*','rule-xtra','$STRING'("About to sprout ~A"),'*reality*'],[setq,'*reality*',['cx$sprout','*reality*']],['ndbg-roman-nl','*gate-dbg*','rule-xtra','$STRING'("Back from sprouting")],[setq,'*reality-lookahead*','*reality*'],['clear-subgoals','resolved-goal','*reality*','*me-belief-path*']])
wl:lambda_def(defun, u_terminate_top_level_goal, f_u_terminate_top_level_goal, [u_top_level_goal, u_resolved_goal, u_resolution_context], [[u_reality_stabilize], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Terminating planning for top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_task_print_plans, u_top_level_goal], [u_ob_c36_set, u_resolved_goal, [quote, u_termination_context], u_resolution_context], [setq, u_xx_top_level_goals_xx, [u_delq_c33, u_top_level_goal, u_xx_top_level_goals_xx]], [u_print_tasks], [u_remove_motivating_emotions, u_xx_reality_xx, u_resolved_goal], [if, [or, [not, [u_dd_goal_c63, u_resolved_goal]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]], [quote, u_revenge]]], [progn, [u_emotional_response, u_resolved_goal, [], [u_strength, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]], u_resolution_context]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_episode_store_top_goal, u_resolved_goal, u_resolution_context]], [if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_no_gen, [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_cx_c36_assert, u_xx_reality_xx, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]]]]], [if, [and, [u_eq_c63, [quote, real], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_failed_goal]]], [u_no_gen, [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "About to sprout ~A"), u_xx_reality_xx], [setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_reality_xx]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Back from sprouting")], [setq, u_xx_reality_lookahead_xx, u_xx_reality_xx], [u_clear_subgoals, u_resolved_goal, u_xx_reality_xx, u_xx_me_belief_path_xx]]).
wl:arglist_info(u_terminate_top_level_goal, f_u_terminate_top_level_goal, [u_top_level_goal, u_resolved_goal, u_resolution_context], arginfo{all:[u_top_level_goal, u_resolved_goal, u_resolution_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_resolved_goal, u_resolution_context], opt:0, req:[u_top_level_goal, u_resolved_goal, u_resolution_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_terminate_top_level_goal).

/*

### Compiled:  `U::TERMINATE-TOP-LEVEL-GOAL` 
*/
f_u_terminate_top_level_goal(Top_level_goal, Resolved_goal, Resolution_context, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_top_level_goal, Top_level_goal), bv(u_resolved_goal, Resolved_goal), bv(u_resolution_context, Resolution_context)|Env],
	f_u_reality_stabilize(Reality_stabilize_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  "Terminating planning for top-level goal")
		       ],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*], claz_base_character, " ~A"),
			 u_top_level_goal
		       ],
		       Ndbg_roman_Ret52),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	get_var(AEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_task_print_plans(Top_level_goal_Get, Print_plans_Ret),
	get_var(AEnv, u_resolution_context, Resolution_context_Get),
	get_var(AEnv, u_resolved_goal, Resolved_goal_Get),
	f_u_ob_c36_set(Resolved_goal_Get,
		       u_termination_context,
		       Resolution_context_Get,
		       C36_set_Ret),
	f_u_delq_c33(u_top_level_goal,
		     u_xx_top_level_goals_xx,
		     Xx_top_level_goals_xx),
	set_var(AEnv, u_xx_top_level_goals_xx, Xx_top_level_goals_xx),
	f_u_print_tasks(Print_tasks_Ret),
	get_var(AEnv, u_resolved_goal, Resolved_goal_Get9),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_remove_motivating_emotions(Xx_reality_xx_Get,
				       Resolved_goal_Get9,
				       Motivating_emotions_Ret),
	(   get_var(AEnv, u_resolved_goal, Resolved_goal_Get12),
	    f_u_dd_goal_c63(Resolved_goal_Get12, Not_Param),
	    cl_not(Not_Param, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(AEnv, u_resolved_goal, Resolved_goal_Get13),
	    f_u_ob_c36_get(Resolved_goal_Get13, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_revenge, Revenge),
	    IFTEST=Revenge
	),
	(   IFTEST\==[]
	->  get_var(AEnv, u_resolved_goal, Resolved_goal_Get15),
	    f_u_strength([u_ob_c36_get, u_resolved_goal, [quote, u_obj]],
			 Strength_Ret),
	    get_var(AEnv, u_resolution_context, Resolution_context_Get16),
	    f_u_emotional_response(Resolved_goal_Get15,
				   [],
				   Strength_Ret,
				   Resolution_context_Get16,
				   TrueResult),
	    _173774416=TrueResult
	;   _173774416=[]
	),
	get_var(AEnv, u_resolved_goal, Resolved_goal_Get20),
	f_u_ty_c36_instance_c63(Resolved_goal_Get20, u_succeeded_goal, IFTEST18),
	(   IFTEST18\==[]
	->  get_var(AEnv, u_resolution_context, Resolution_context_Get22),
	    get_var(AEnv, u_resolved_goal, Resolved_goal_Get21),
	    f_u_episode_store_top_goal(Resolved_goal_Get21,
				       Resolution_context_Get22,
				       TrueResult23),
	    _173810856=TrueResult23
	;   _173810856=[]
	),
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST24),
	(   IFTEST24\==[]
	->  f_u_no_gen(
		       [ [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal],
			 
			 [ if,
			   
			   [ u_ty_c36_instance_c63,
			     u_resolved_goal,
			     [quote, u_succeeded_goal]
			   ],
			   
			   [ u_cx_c36_assert,
			     u_xx_reality_xx,
			     [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]
			   ]
			 ]
		       ],
		       TrueResult26),
	    _173822326=TrueResult26
	;   _173822326=[]
	),
	f_u_eq_c63([quote, real],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST29),
	(   IFTEST29\==[]
	->  get_var(AEnv, u_resolved_goal, Resolved_goal_Get31),
	    f_u_ty_c36_instance_c63(Resolved_goal_Get31,
				    u_failed_goal,
				    TrueResult32),
	    IFTEST27=TrueResult32
	;   IFTEST27=[]
	),
	(   IFTEST27\==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal]],
		       TrueResult33),
	    _173827790=TrueResult33
	;   _173827790=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "About to sprout ~A"),
			    u_xx_reality_xx
			  ],
			  Roman_nl_Ret),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get34),
	f_u_cx_c36_sprout(Xx_reality_xx_Get34, Xx_reality_xx),
	set_var(AEnv, u_xx_reality_xx, Xx_reality_xx),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Back from sprouting")
			  ],
			  Roman_nl_Ret59),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get35),
	set_var(AEnv, u_xx_reality_lookahead_xx, Xx_reality_xx_Get35),
	get_var(AEnv, u_resolved_goal, Resolved_goal_Get36),
	get_var(AEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(AEnv, u_xx_reality_xx, Xx_reality_xx_Get37),
	f_u_clear_subgoals(Resolved_goal_Get36,
			   Xx_reality_xx_Get37,
			   Xx_me_belief_path_xx_Get,
			   Clear_subgoals_Ret),
	Clear_subgoals_Ret=FnResult.
:- set_opv(f_u_terminate_top_level_goal, classof, claz_function),
   set_opv(u_terminate_top_level_goal, compile_as, kw_function),
   set_opv(u_terminate_top_level_goal, function, f_u_terminate_top_level_goal),
   DefunResult=u_terminate_top_level_goal.
/*
:- side_effect(assert_lsp(u_terminate_top_level_goal,
			  wl:lambda_def(defun, u_terminate_top_level_goal, f_u_terminate_top_level_goal, [u_top_level_goal, u_resolved_goal, u_resolution_context], [[u_reality_stabilize], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Terminating planning for top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_task_print_plans, u_top_level_goal], [u_ob_c36_set, u_resolved_goal, [quote, u_termination_context], u_resolution_context], [setq, u_xx_top_level_goals_xx, [u_delq_c33, u_top_level_goal, u_xx_top_level_goals_xx]], [u_print_tasks], [u_remove_motivating_emotions, u_xx_reality_xx, u_resolved_goal], [if, [or, [not, [u_dd_goal_c63, u_resolved_goal]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]], [quote, u_revenge]]], [progn, [u_emotional_response, u_resolved_goal, [], [u_strength, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]], u_resolution_context]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_episode_store_top_goal, u_resolved_goal, u_resolution_context]], [if, [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_no_gen, [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_cx_c36_assert, u_xx_reality_xx, [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]]]]], [if, [and, [u_eq_c63, [quote, real], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_failed_goal]]], [u_no_gen, [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "About to sprout ~A"), u_xx_reality_xx], [setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_reality_xx]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Back from sprouting")], [setq, u_xx_reality_lookahead_xx, u_xx_reality_xx], [u_clear_subgoals, u_resolved_goal, u_xx_reality_xx, u_xx_me_belief_path_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_terminate_top_level_goal,
			  wl:arglist_info(u_terminate_top_level_goal, f_u_terminate_top_level_goal, [u_top_level_goal, u_resolved_goal, u_resolution_context], arginfo{all:[u_top_level_goal, u_resolved_goal, u_resolution_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_resolved_goal, u_resolution_context], opt:0, req:[u_top_level_goal, u_resolved_goal, u_resolution_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_terminate_top_level_goal,
			  wl:init_args(exact_only, f_u_terminate_top_level_goal))).
*/
/*
 necessary?
*/
/*
 Stop any future planning on this top-level goal
*/
/*
 Update to trace the list of tasks
*/
/*
 Remove emotional motivators for active goal (which got carried
*/
/*
 over to resolved goal)
*/
/*
 Invoke general emotional response
*/
/*
       (gen-overall-emot-state) Now done in dd_gen
*/
/*
 Store the successful planning episode in episodic memory
*/
/*
 If top-level goal was imaginary planning, assert final goal status
*/
/*
 (and objective if goal success) into reality context.
*/
/*
 If top-level goal was real planning and goal failure, we better assert
*/
/*
 the failure into the reality context in case it isn't already there.
*/
/*
 (gc-plans1 *reality* (list top-level-goal))
*/
/*
 If we use gc-plans1 here, we must modify it so it does not retract
*/
/*
 side-effect goal outcomes.
*/
/*
(defun remove-motivating-emotions (context goal)
  (ndbg-roman-nl *gate-dbg* rule "Removing motivating emotions of "(defun remove-motivating-emotions (context goal)\n  (ndbg-roman-nl *gate-dbg* rule \"Removing motivating emotions of ~A in ~A\"\n                        goal context)\n  (let ((dependencies (get-dependencies goal context *me-belief-path*)))\n       (yloop (yfor dependency in dependencies)\n              (ydo \n               (if (ty$instance? (ob$get dependency 'linked-from) 'emotion)\n                   (progn\n                    (cx$retract context dependency)\n                    (remove-if-free (ob$get dependency 'linked-from)\n                                    context)))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:15940 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'remove-motivating-emotions',[context,goal],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Removing motivating emotions of ~A in ~A"),goal,context],[let,[[dependencies,['get-dependencies',goal,context,'*me-belief-path*']]],[yloop,[yfor,dependency,in,dependencies],[ydo,[if,['ty$instance?',['ob$get',dependency,[quote,'linked-from']],[quote,emotion]],[progn,['cx$retract',context,dependency],['remove-if-free',['ob$get',dependency,[quote,'linked-from']],context]]]]]]])
wl:lambda_def(defun, u_remove_motivating_emotions, f_u_remove_motivating_emotions, [u_context, u_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Removing motivating emotions of ~A in ~A"), u_goal, u_context], [let, [[u_dependencies, [u_get_dependencies, u_goal, u_context, u_xx_me_belief_path_xx]]], [u_yloop, [u_yfor, u_dependency, u_in, u_dependencies], [u_ydo, [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_dependency, [quote, u_linked_from]], [quote, u_emotion]], [progn, [u_cx_c36_retract, u_context, u_dependency], [u_remove_if_free, [u_ob_c36_get, u_dependency, [quote, u_linked_from]], u_context]]]]]]]).
wl:arglist_info(u_remove_motivating_emotions, f_u_remove_motivating_emotions, [u_context, u_goal], arginfo{all:[u_context, u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_goal], opt:0, req:[u_context, u_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_remove_motivating_emotions).

/*

### Compiled:  `U::REMOVE-MOTIVATING-EMOTIONS` 
*/
f_u_remove_motivating_emotions(Context, Goal, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_context, Context), bv(u_goal, Goal)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Removing motivating emotions of ~A in ~A"),
			    u_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	get_var(Env13, u_context, Context_Get),
	get_var(Env13, u_goal, Goal_Get),
	get_var(Env13, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_get_dependencies(Goal_Get,
			     Context_Get,
			     Xx_me_belief_path_xx_Get,
			     Dependencies_Init),
	LEnv=[bv(u_dependencies, Dependencies_Init)|Env13],
	f_u_yloop(
		  [ [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			  [quote, u_emotion]
			],
			
			[ progn,
			  [u_cx_c36_retract, u_context, u_dependency],
			  
			  [ u_remove_if_free,
			    [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			    u_context
			  ]
			]
		      ]
		    ]
		  ],
		  LetResult),
	LetResult=FnResult.
:- set_opv(f_u_remove_motivating_emotions, classof, claz_function),
   set_opv(u_remove_motivating_emotions, compile_as, kw_function),
   set_opv(u_remove_motivating_emotions,
	   function,
	   f_u_remove_motivating_emotions),
   DefunResult=u_remove_motivating_emotions.
/*
:- side_effect(assert_lsp(u_remove_motivating_emotions,
			  wl:lambda_def(defun, u_remove_motivating_emotions, f_u_remove_motivating_emotions, [u_context, u_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Removing motivating emotions of ~A in ~A"), u_goal, u_context], [let, [[u_dependencies, [u_get_dependencies, u_goal, u_context, u_xx_me_belief_path_xx]]], [u_yloop, [u_yfor, u_dependency, u_in, u_dependencies], [u_ydo, [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_dependency, [quote, u_linked_from]], [quote, u_emotion]], [progn, [u_cx_c36_retract, u_context, u_dependency], [u_remove_if_free, [u_ob_c36_get, u_dependency, [quote, u_linked_from]], u_context]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_remove_motivating_emotions,
			  wl:arglist_info(u_remove_motivating_emotions, f_u_remove_motivating_emotions, [u_context, u_goal], arginfo{all:[u_context, u_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_goal], opt:0, req:[u_context, u_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_remove_motivating_emotions,
			  wl:init_args(exact_only, f_u_remove_motivating_emotions))).
*/
/*
(defun remove-if-free (emotion context)
  (let ((dependees (get-dependees emotion context *me-belief-path*))
        (dependencies (get-dependencies emotion context *me-belief-path*)))
       (if (and (null? dependees)
                (null? dependencies))
           (progn
            (cx$retract context emotion)
            (emotion-delete emotion)))))

; Assumes reality is stabilized.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16519 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'remove-if-free',[emotion,context],[let,[[dependees,['get-dependees',emotion,context,'*me-belief-path*']],[dependencies,['get-dependencies',emotion,context,'*me-belief-path*']]],[if,[and,['null?',dependees],['null?',dependencies]],[progn,['cx$retract',context,emotion],['emotion-delete',emotion]]]]])
wl:lambda_def(defun, u_remove_if_free, f_u_remove_if_free, [u_emotion, u_context], [[let, [[u_dependees, [u_get_dependees, u_emotion, u_context, u_xx_me_belief_path_xx]], [u_dependencies, [u_get_dependencies, u_emotion, u_context, u_xx_me_belief_path_xx]]], [if, [and, [u_null_c63, u_dependees], [u_null_c63, u_dependencies]], [progn, [u_cx_c36_retract, u_context, u_emotion], [u_emotion_delete, u_emotion]]]]]).
wl:arglist_info(u_remove_if_free, f_u_remove_if_free, [u_emotion, u_context], arginfo{all:[u_emotion, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emotion, u_context], opt:0, req:[u_emotion, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_remove_if_free).

/*

### Compiled:  `U::REMOVE-IF-FREE` 
*/
f_u_remove_if_free(Emotion, Context, FnResult) :-
	nop(global_env(Env)),
	Env26=[bv(u_emotion, Emotion), bv(u_context, Context)|Env],
	get_var(Env26, u_context, Context_Get),
	get_var(Env26, u_emotion, Emotion_Get),
	get_var(Env26, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_get_dependees(Emotion_Get,
			  Context_Get,
			  Xx_me_belief_path_xx_Get,
			  Dependees_Init),
	get_var(Env26, u_context, Context_Get11),
	get_var(Env26, u_emotion, Emotion_Get10),
	get_var(Env26, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get12),
	f_u_get_dependencies(Emotion_Get10,
			     Context_Get11,
			     Xx_me_belief_path_xx_Get12,
			     Dependencies_Init),
	LEnv=[bv(u_dependees, Dependees_Init), bv(u_dependencies, Dependencies_Init)|Env26],
	f_u_null_c63(u_dependees, IFTEST17),
	(   IFTEST17\==[]
	->  f_u_null_c63(u_dependencies, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_context, Context_Get20),
	    get_var(LEnv, u_emotion, Emotion_Get21),
	    f_u_cx_c36_retract(Context_Get20, Emotion_Get21, C36_retract_Ret),
	    get_var(LEnv, u_emotion, Emotion_Get22),
	    f_u_emotion_delete(Emotion_Get22, TrueResult23),
	    FnResult=TrueResult23
	;   FnResult=[]
	).
:- set_opv(f_u_remove_if_free, classof, claz_function),
   set_opv(u_remove_if_free, compile_as, kw_function),
   set_opv(u_remove_if_free, function, f_u_remove_if_free),
   DefunResult=u_remove_if_free.
/*
:- side_effect(assert_lsp(u_remove_if_free,
			  wl:lambda_def(defun, u_remove_if_free, f_u_remove_if_free, [u_emotion, u_context], [[let, [[u_dependees, [u_get_dependees, u_emotion, u_context, u_xx_me_belief_path_xx]], [u_dependencies, [u_get_dependencies, u_emotion, u_context, u_xx_me_belief_path_xx]]], [if, [and, [u_null_c63, u_dependees], [u_null_c63, u_dependencies]], [progn, [u_cx_c36_retract, u_context, u_emotion], [u_emotion_delete, u_emotion]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_remove_if_free,
			  wl:arglist_info(u_remove_if_free, f_u_remove_if_free, [u_emotion, u_context], arginfo{all:[u_emotion, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emotion, u_context], opt:0, req:[u_emotion, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_remove_if_free,
			  wl:init_args(exact_only, f_u_remove_if_free))).
*/
/*
 Assumes reality is stabilized.
*/
/*
(defun emotional-response (resolved-goal altern? realism context)
  (ndbg-roman-nl *gate-dbg* rule "Emotional responses for "(defun emotional-response (resolved-goal altern? realism context)\n  (ndbg-roman-nl *gate-dbg* rule \"Emotional responses for ~A in ~A\"\n                        resolved-goal context)\n  ; Actually, as coded currently, get-other-causes returns a bd list\n  ; (whose car is an other cause).\n  (let* ((other-causes (get-other-causes resolved-goal context))\n         (emot nil))\n        (setq emot\n              (if other-causes\n                  (if altern?\n                      (if (ty$instance? resolved-goal 'succeeded-goal)\n                          ; angry at someone for not doing something\n                          ; Should we substitute function 'first-non-me'\n                          ; which is used in generator? No, other-causes\n                          ; will not contain a me.\n                          (ob$fcreate `(NEG-EMOTION to ,(ob$get\n                                                         (car other-causes)\n                                                         'actor)\n                                                    altern? t))\n                          (ob$fcreate '(POS-EMOTION altern? t)))\n                      (if (ty$instance? resolved-goal 'succeeded-goal)\n                          (ob$fcreate '(POS-EMOTION))\n                          (ob$fcreate `(NEG-EMOTION to ,(ob$get (car\n                                                           other-causes)\n                                                          'actor)))))\n                  (if altern?\n                      (if (ty$instance? resolved-goal 'succeeded-goal)\n                          (ob$fcreate '(NEG-EMOTION altern? t))\n                          (ob$fcreate '(POS-EMOTION altern? t)))\n                      (if (ty$instance? resolved-goal 'succeeded-goal)\n                          (ob$fcreate '(POS-EMOTION))\n                          (ob$fcreate '(NEG-EMOTION))))))\n        (add-emotion resolved-goal emot realism context)\n        emot))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'emotional-response',['resolved-goal','altern?',realism,context],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Emotional responses for ~A in ~A"),'resolved-goal',context],['let*',[['other-causes',['get-other-causes','resolved-goal',context]],[emot,[]]],[setq,emot,[if,'other-causes',[if,'altern?',[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['ob$fcreate',['#BQ',['NEG-EMOTION',to,['#COMMA',['ob$get',[car,'other-causes'],[quote,actor]]],'altern?',t]]],['ob$fcreate',[quote,['POS-EMOTION','altern?',t]]]],[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['ob$fcreate',[quote,['POS-EMOTION']]],['ob$fcreate',['#BQ',['NEG-EMOTION',to,['#COMMA',['ob$get',[car,'other-causes'],[quote,actor]]]]]]]],[if,'altern?',[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['ob$fcreate',[quote,['NEG-EMOTION','altern?',t]]],['ob$fcreate',[quote,['POS-EMOTION','altern?',t]]]],[if,['ty$instance?','resolved-goal',[quote,'succeeded-goal']],['ob$fcreate',[quote,['POS-EMOTION']]],['ob$fcreate',[quote,['NEG-EMOTION']]]]]]],['add-emotion','resolved-goal',emot,realism,context],emot]])
wl:lambda_def(defun, u_emotional_response, f_u_emotional_response, [u_resolved_goal, u_altern_c63, u_realism, u_context], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Emotional responses for ~A in ~A"), u_resolved_goal, u_context], [let_xx, [[u_other_causes, [u_get_other_causes, u_resolved_goal, u_context]], [u_emot, []]], [setq, u_emot, [if, u_other_causes, [if, u_altern_c63, [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, ['#BQ', [u_neg_emotion, u_to, ['#COMMA', [u_ob_c36_get, [car, u_other_causes], [quote, u_actor]]], u_altern_c63, t]]], [u_ob_c36_fcreate, [quote, [u_pos_emotion, u_altern_c63, t]]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_pos_emotion]]], [u_ob_c36_fcreate, ['#BQ', [u_neg_emotion, u_to, ['#COMMA', [u_ob_c36_get, [car, u_other_causes], [quote, u_actor]]]]]]]], [if, u_altern_c63, [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_neg_emotion, u_altern_c63, t]]], [u_ob_c36_fcreate, [quote, [u_pos_emotion, u_altern_c63, t]]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_pos_emotion]]], [u_ob_c36_fcreate, [quote, [u_neg_emotion]]]]]]], [u_add_emotion, u_resolved_goal, u_emot, u_realism, u_context], u_emot]]).
wl:arglist_info(u_emotional_response, f_u_emotional_response, [u_resolved_goal, u_altern_c63, u_realism, u_context], arginfo{all:[u_resolved_goal, u_altern_c63, u_realism, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_resolved_goal, u_altern_c63, u_realism, u_context], opt:0, req:[u_resolved_goal, u_altern_c63, u_realism, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_emotional_response).

/*

### Compiled:  `U::EMOTIONAL-RESPONSE` 
*/
f_u_emotional_response(Resolved_goal, Altern_c63, Realism, Context, FnResult) :-
	nop(global_env(Env)),
	Env53=[bv(u_resolved_goal, Resolved_goal), bv(u_altern_c63, Altern_c63), bv(u_realism, Realism), bv(u_context, Context)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Emotional responses for ~A in ~A"),
			    u_resolved_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	get_var(Env53, u_context, Context_Get),
	get_var(Env53, u_resolved_goal, Resolved_goal_Get),
	f_u_get_other_causes(Resolved_goal_Get, Context_Get, Other_causes_Init),
	LEnv=[bv(u_other_causes, Other_causes_Init), bv(u_emot, [])|Env53],
	get_var(LEnv, u_other_causes, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_altern_c63, IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(LEnv, u_resolved_goal, Resolved_goal_Get19),
		f_u_ty_c36_instance_c63(Resolved_goal_Get19,
					u_succeeded_goal,
					IFTEST17),
		(   IFTEST17\==[]
		->  f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_neg_emotion,
					   u_to,
					   
					   [ '#COMMA',
					     
					     [ u_ob_c36_get,
					       [car, u_other_causes],
					       [quote, u_actor]
					     ]
					   ],
					   u_altern_c63,
					   t
					 ]
				       ],
				       TrueResult),
		    TrueResult27=TrueResult
		;   f_u_ob_c36_fcreate([quote, [u_pos_emotion, u_altern_c63, t]],
				       ElseResult),
		    TrueResult27=ElseResult
		)
	    ;   get_var(LEnv, u_resolved_goal, Resolved_goal_Get24),
		f_u_ty_c36_instance_c63(Resolved_goal_Get24,
					u_succeeded_goal,
					IFTEST22),
		(   IFTEST22\==[]
		->  f_u_ob_c36_fcreate([quote, [u_pos_emotion]], TrueResult25),
		    TrueResult27=TrueResult25
		;   f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_neg_emotion,
					   u_to,
					   
					   [ '#COMMA',
					     
					     [ u_ob_c36_get,
					       [car, u_other_causes],
					       [quote, u_actor]
					     ]
					   ]
					 ]
				       ],
				       ElseResult26),
		    TrueResult27=ElseResult26
		)
	    )
	;   get_var(LEnv, u_altern_c63, IFTEST29),
	    (   IFTEST29\==[]
	    ->  get_var(LEnv, u_resolved_goal, Resolved_goal_Get34),
		f_u_ty_c36_instance_c63(Resolved_goal_Get34,
					u_succeeded_goal,
					IFTEST32),
		(   IFTEST32\==[]
		->  f_u_ob_c36_fcreate([quote, [u_neg_emotion, u_altern_c63, t]],
				       TrueResult35),
		    TrueResult27=TrueResult35
		;   f_u_ob_c36_fcreate([quote, [u_pos_emotion, u_altern_c63, t]],
				       ElseResult36),
		    TrueResult27=ElseResult36
		)
	    ;   get_var(LEnv, u_resolved_goal, Resolved_goal_Get39),
		f_u_ty_c36_instance_c63(Resolved_goal_Get39,
					u_succeeded_goal,
					IFTEST37),
		(   IFTEST37\==[]
		->  f_u_ob_c36_fcreate([quote, [u_pos_emotion]], TrueResult40),
		    TrueResult27=TrueResult40
		;   f_u_ob_c36_fcreate([quote, [u_neg_emotion]], ElseResult41),
		    TrueResult27=ElseResult41
		)
	    )
	),
	set_var(LEnv, u_emot, TrueResult27),
	get_var(LEnv, u_context, Context_Get49),
	get_var(LEnv, u_emot, Emot_Get),
	get_var(LEnv, u_realism, Realism_Get),
	get_var(LEnv, u_resolved_goal, Resolved_goal_Get46),
	f_u_add_emotion(Resolved_goal_Get46,
			Emot_Get,
			Realism_Get,
			Context_Get49,
			Add_emotion_Ret),
	get_var(LEnv, u_emot, Emot_Get50),
	Emot_Get50=FnResult.
:- set_opv(f_u_emotional_response, classof, claz_function),
   set_opv(u_emotional_response, compile_as, kw_function),
   set_opv(u_emotional_response, function, f_u_emotional_response),
   DefunResult=u_emotional_response.
/*
:- side_effect(assert_lsp(u_emotional_response,
			  wl:lambda_def(defun, u_emotional_response, f_u_emotional_response, [u_resolved_goal, u_altern_c63, u_realism, u_context], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Emotional responses for ~A in ~A"), u_resolved_goal, u_context], [let_xx, [[u_other_causes, [u_get_other_causes, u_resolved_goal, u_context]], [u_emot, []]], [setq, u_emot, [if, u_other_causes, [if, u_altern_c63, [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, ['#BQ', [u_neg_emotion, u_to, ['#COMMA', [u_ob_c36_get, [car, u_other_causes], [quote, u_actor]]], u_altern_c63, t]]], [u_ob_c36_fcreate, [quote, [u_pos_emotion, u_altern_c63, t]]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_pos_emotion]]], [u_ob_c36_fcreate, ['#BQ', [u_neg_emotion, u_to, ['#COMMA', [u_ob_c36_get, [car, u_other_causes], [quote, u_actor]]]]]]]], [if, u_altern_c63, [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_neg_emotion, u_altern_c63, t]]], [u_ob_c36_fcreate, [quote, [u_pos_emotion, u_altern_c63, t]]]], [if, [u_ty_c36_instance_c63, u_resolved_goal, [quote, u_succeeded_goal]], [u_ob_c36_fcreate, [quote, [u_pos_emotion]]], [u_ob_c36_fcreate, [quote, [u_neg_emotion]]]]]]], [u_add_emotion, u_resolved_goal, u_emot, u_realism, u_context], u_emot]]))).
*/
/*
:- side_effect(assert_lsp(u_emotional_response,
			  wl:arglist_info(u_emotional_response, f_u_emotional_response, [u_resolved_goal, u_altern_c63, u_realism, u_context], arginfo{all:[u_resolved_goal, u_altern_c63, u_realism, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_resolved_goal, u_altern_c63, u_realism, u_context], opt:0, req:[u_resolved_goal, u_altern_c63, u_realism, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_emotional_response,
			  wl:init_args(exact_only, f_u_emotional_response))).
*/
/*
 Actually, as coded currently, get-other-causes returns a bd list
*/
/*
 (whose car is an other cause).
*/
/*
 angry at someone for not doing something
*/
/*
 Should we substitute function 'first-non-me'
*/
/*
 which is used in generator? No, other-causes
*/
/*
 will not contain a me.
*/
/*
(setq *emotions* nil)

; Note that goal may not be in *reality*. Will this cause inconsistencies
; later?
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18855 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*emotions*',[]])
:- set_var(AEnv, setq, u_xx_emotions_xx, []).
/*
 Note that goal may not be in *reality*. Will this cause inconsistencies
*/
/*
 later?
*/
/*
(defun add-emotion (goal emotion realism context)
  (emotion-add emotion)
  (add-depend *reality* goal emotion realism 0.0 0.0 nil)
  (cx$assert *reality* emotion)
  ; The purpose of the below is to activate any daydreaming goals
  ; that should be activated as a result of the new emotion.
  (if (neq? *reality* context)
      (progn
       (ndbg-roman-nl *gate-dbg* rule-long "Running reality inferences")
       (run-inferences *reality* nil *me-belief-path*))))

; This is called from run-inferences.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18961 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-emotion',[goal,emotion,realism,context],['emotion-add',emotion],['add-depend','*reality*',goal,emotion,realism,0.0,0.0,[]],['cx$assert','*reality*',emotion],[if,['neq?','*reality*',context],[progn,['ndbg-roman-nl','*gate-dbg*','rule-long','$STRING'("Running reality inferences")],['run-inferences','*reality*',[],'*me-belief-path*']]]])
wl:lambda_def(defun, u_add_emotion, f_u_add_emotion, [u_goal, u_emotion, u_realism, u_context], [[u_emotion_add, u_emotion], [u_add_depend, u_xx_reality_xx, u_goal, u_emotion, u_realism, 0.0, 0.0, []], [u_cx_c36_assert, u_xx_reality_xx, u_emotion], [if, [u_neq_c63, u_xx_reality_xx, u_context], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Running reality inferences")], [u_run_inferences, u_xx_reality_xx, [], u_xx_me_belief_path_xx]]]]).
wl:arglist_info(u_add_emotion, f_u_add_emotion, [u_goal, u_emotion, u_realism, u_context], arginfo{all:[u_goal, u_emotion, u_realism, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_emotion, u_realism, u_context], opt:0, req:[u_goal, u_emotion, u_realism, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_add_emotion).

/*

### Compiled:  `U::ADD-EMOTION` 
*/
f_u_add_emotion(Goal, Emotion, Realism, Context, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_goal, Goal), bv(u_emotion, Emotion), bv(u_realism, Realism), bv(u_context, Context)|Env],
	get_var(Env18, u_emotion, Emotion_Get),
	f_u_emotion_add(Emotion_Get, Emotion_add_Ret),
	get_var(Env18, u_emotion, Emotion_Get7),
	get_var(Env18, u_goal, Goal_Get),
	get_var(Env18, u_realism, Realism_Get),
	get_var(Env18, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_add_depend(Xx_reality_xx_Get,
		       Goal_Get,
		       Emotion_Get7,
		       Realism_Get,
		       0.0,
		       0.0,
		       [],
		       Add_depend_Ret),
	get_var(Env18, u_emotion, Emotion_Get10),
	get_var(Env18, u_xx_reality_xx, Xx_reality_xx_Get9),
	f_u_cx_c36_assert(Xx_reality_xx_Get9, Emotion_Get10, C36_assert_Ret),
	f_u_neq_c63(u_xx_reality_xx, u_context, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Running reality inferences")
			      ],
			      Roman_nl_Ret),
	    get_var(Env18, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    get_var(Env18, u_xx_reality_xx, Xx_reality_xx_Get13),
	    f_u_run_inferences(Xx_reality_xx_Get13,
			       [],
			       Xx_me_belief_path_xx_Get,
			       TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_add_emotion, classof, claz_function),
   set_opv(u_add_emotion, compile_as, kw_function),
   set_opv(u_add_emotion, function, f_u_add_emotion),
   DefunResult=u_add_emotion.
/*
:- side_effect(assert_lsp(u_add_emotion,
			  wl:lambda_def(defun, u_add_emotion, f_u_add_emotion, [u_goal, u_emotion, u_realism, u_context], [[u_emotion_add, u_emotion], [u_add_depend, u_xx_reality_xx, u_goal, u_emotion, u_realism, 0.0, 0.0, []], [u_cx_c36_assert, u_xx_reality_xx, u_emotion], [if, [u_neq_c63, u_xx_reality_xx, u_context], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Running reality inferences")], [u_run_inferences, u_xx_reality_xx, [], u_xx_me_belief_path_xx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_emotion,
			  wl:arglist_info(u_add_emotion, f_u_add_emotion, [u_goal, u_emotion, u_realism, u_context], arginfo{all:[u_goal, u_emotion, u_realism, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_emotion, u_realism, u_context], opt:0, req:[u_goal, u_emotion, u_realism, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_emotion,
			  wl:init_args(exact_only, f_u_add_emotion))).
*/
/*
 The purpose of the below is to activate any daydreaming goals
*/
/*
 that should be activated as a result of the new emotion.
*/
/*
 This is called from run-inferences.
*/
/*
(defun personal-goal-outcome (goal context top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Personal goal outcome "(defun personal-goal-outcome (goal context top-level-goal)\n  (ndbg-roman-nl *gate-dbg* rule \"Personal goal outcome ~A in ~A\" goal context)\n  (reality-stabilize) ; yes?\n  (ob$set goal 'termination-context context)\n  (let ((emot (emotional-response goal (altern? context)\n                                  (strength (ob$get goal 'obj)) context)))\n       (if (ty$instance? (ob$get top-level-goal 'obj) 'rationalization)\n           (divert-emot-to-tlg emot context top-level-goal))\n;       (gen-overall-emot-state) Now done in dd_gen\n))\n\n; Divert strength of new emotion to main motivator of a top-level goal.\n; Used for rationalization and surprise.\n;  Why do we have to do it this way? Why do we have to divert to the\n; main motivator? Why can't we just connect a new emotion up to the\n;  goal and recalculate its value? Was this because of something that is\n; no longer true?\n; The reason is that 1) Rationalization-Inf1 works off of the emotion\n; which originally activated rationalization; 2) we actually want\n; an initial NEG-EMOTION to be nulled out of existence through\n; rationalization rather than simply summing in POS-EMOTIONs to\n; counteract.\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'personal-goal-outcome',[goal,context,'top-level-goal'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Personal goal outcome ~A in ~A"),goal,context],['reality-stabilize'],['ob$set',goal,[quote,'termination-context'],context],[let,[[emot,['emotional-response',goal,['altern?',context],[strength,['ob$get',goal,[quote,obj]]],context]]],[if,['ty$instance?',['ob$get','top-level-goal',[quote,obj]],[quote,rationalization]],['divert-emot-to-tlg',emot,context,'top-level-goal']]]])
wl:lambda_def(defun, u_personal_goal_outcome, f_u_personal_goal_outcome, [u_goal, u_context, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Personal goal outcome ~A in ~A"), u_goal, u_context], [u_reality_stabilize], [u_ob_c36_set, u_goal, [quote, u_termination_context], u_context], [let, [[u_emot, [u_emotional_response, u_goal, [u_altern_c63, u_context], [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]], u_context]]], [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_top_level_goal, [quote, u_obj]], [quote, u_rationalization]], [u_divert_emot_to_tlg, u_emot, u_context, u_top_level_goal]]]]).
wl:arglist_info(u_personal_goal_outcome, f_u_personal_goal_outcome, [u_goal, u_context, u_top_level_goal], arginfo{all:[u_goal, u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_top_level_goal], opt:0, req:[u_goal, u_context, u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_personal_goal_outcome).

/*

### Compiled:  `U::PERSONAL-GOAL-OUTCOME` 
*/
f_u_personal_goal_outcome(Goal, Context, Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_goal, Goal), bv(u_context, Context), bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Personal goal outcome ~A in ~A"),
			    u_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_reality_stabilize(Reality_stabilize_Ret),
	get_var(Env22, u_context, Context_Get),
	get_var(Env22, u_goal, Goal_Get),
	f_u_ob_c36_set(Goal_Get, u_termination_context, Context_Get, C36_set_Ret),
	get_var(Env22, u_context, Context_Get10),
	get_var(Env22, u_goal, Goal_Get9),
	f_u_altern_c63(Context_Get10, Altern_c63_Ret),
	f_u_strength([u_ob_c36_get, u_goal, [quote, u_obj]], Strength_Ret),
	get_var(Env22, u_context, Context_Get11),
	f_u_emotional_response(Goal_Get9,
			       Altern_c63_Ret,
			       Strength_Ret,
			       Context_Get11,
			       Emot_Init),
	LEnv=[bv(u_emot, Emot_Init)|Env22],
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_ob_c36_get(Top_level_goal_Get, u_obj, Obj),
	f_u_ty_c36_instance_c63(Obj, u_rationalization, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_context, Context_Get17),
	    get_var(LEnv, u_emot, Emot_Get),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get18),
	    f_u_divert_emot_to_tlg(Emot_Get,
				   Context_Get17,
				   Top_level_goal_Get18,
				   TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_personal_goal_outcome, classof, claz_function),
   set_opv(u_personal_goal_outcome, compile_as, kw_function),
   set_opv(u_personal_goal_outcome, function, f_u_personal_goal_outcome),
   DefunResult=u_personal_goal_outcome.
/*
:- side_effect(assert_lsp(u_personal_goal_outcome,
			  wl:lambda_def(defun, u_personal_goal_outcome, f_u_personal_goal_outcome, [u_goal, u_context, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Personal goal outcome ~A in ~A"), u_goal, u_context], [u_reality_stabilize], [u_ob_c36_set, u_goal, [quote, u_termination_context], u_context], [let, [[u_emot, [u_emotional_response, u_goal, [u_altern_c63, u_context], [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]], u_context]]], [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_top_level_goal, [quote, u_obj]], [quote, u_rationalization]], [u_divert_emot_to_tlg, u_emot, u_context, u_top_level_goal]]]]))).
*/
/*
:- side_effect(assert_lsp(u_personal_goal_outcome,
			  wl:arglist_info(u_personal_goal_outcome, f_u_personal_goal_outcome, [u_goal, u_context, u_top_level_goal], arginfo{all:[u_goal, u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_top_level_goal], opt:0, req:[u_goal, u_context, u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_personal_goal_outcome,
			  wl:init_args(exact_only, f_u_personal_goal_outcome))).
*/
/*
 yes?
*/
/*
       (gen-overall-emot-state) Now done in dd_gen
*/
/*
 Divert strength of new emotion to main motivator of a top-level goal.
*/
/*
 Used for rationalization and surprise.
*/
/*
  Why do we have to do it this way? Why do we have to divert to the
*/
/*
 main motivator? Why can't we just connect a new emotion up to the
*/
/*
  goal and recalculate its value? Was this because of something that is
*/
/*
 no longer true?
*/
/*
 The reason is that 1) Rationalization-Inf1 works off of the emotion
*/
/*
 which originally activated rationalization; 2) we actually want
*/
/*
 an initial NEG-EMOTION to be nulled out of existence through
*/
/*
 rationalization rather than simply summing in POS-EMOTIONs to
*/
/*
 counteract.
*/
/*
(defun divert-emot-to-tlg (emot context top-level-goal)
  (let ((main-motiv (ob$get top-level-goal 'main-motiv)))
       (ndbg-roman-nl *gate-dbg* rule "Divert strength of "(defun divert-emot-to-tlg (emot context top-level-goal)\n  (let ((main-motiv (ob$get top-level-goal 'main-motiv)))\n       (ndbg-roman-nl *gate-dbg* rule \"Divert strength of ~A to ~A\"\n                      emot top-level-goal)\n       (modify-strength *reality* main-motiv (strength emot)\n                        (sign-correction emot main-motiv))\n       (ndbg-roman-nl *gate-dbg* rule \"Null out charge of ~A\" emot)\n       (modify-strength *reality* emot (strength emot) -1.0)\n       (print-tasks)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:20618 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'divert-emot-to-tlg',[emot,context,'top-level-goal'],[let,[['main-motiv',['ob$get','top-level-goal',[quote,'main-motiv']]]],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Divert strength of ~A to ~A"),emot,'top-level-goal'],['modify-strength','*reality*','main-motiv',[strength,emot],['sign-correction',emot,'main-motiv']],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Null out charge of ~A"),emot],['modify-strength','*reality*',emot,[strength,emot],-1.0],['print-tasks']]])
wl:lambda_def(defun, u_divert_emot_to_tlg, f_u_divert_emot_to_tlg, [u_emot, u_context, u_top_level_goal], [[let, [[u_main_motiv, [u_ob_c36_get, u_top_level_goal, [quote, u_main_motiv]]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Divert strength of ~A to ~A"), u_emot, u_top_level_goal], [u_modify_strength, u_xx_reality_xx, u_main_motiv, [u_strength, u_emot], [u_sign_correction, u_emot, u_main_motiv]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Null out charge of ~A"), u_emot], [u_modify_strength, u_xx_reality_xx, u_emot, [u_strength, u_emot], -1.0], [u_print_tasks]]]).
wl:arglist_info(u_divert_emot_to_tlg, f_u_divert_emot_to_tlg, [u_emot, u_context, u_top_level_goal], arginfo{all:[u_emot, u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot, u_context, u_top_level_goal], opt:0, req:[u_emot, u_context, u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_divert_emot_to_tlg).

/*

### Compiled:  `U::DIVERT-EMOT-TO-TLG` 
*/
f_u_divert_emot_to_tlg(Emot, Context, Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(u_emot, Emot), bv(u_context, Context), bv(u_top_level_goal, Top_level_goal)|Env],
	get_var(Env17, u_top_level_goal, Top_level_goal_Get),
	f_u_ob_c36_get(Top_level_goal_Get, u_main_motiv, Main_motiv_Init),
	LEnv=[bv(u_main_motiv, Main_motiv_Init)|Env17],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Divert strength of ~A to ~A"),
			    u_emot,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_main_motiv, Main_motiv_Get),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_strength(u_emot, Strength_Ret),
	get_var(LEnv, u_emot, Emot_Get),
	get_var(LEnv, u_main_motiv, Main_motiv_Get12),
	f_u_sign_correction(Emot_Get, Main_motiv_Get12, Sign_correction_Ret),
	f_u_modify_strength(Xx_reality_xx_Get,
			    Main_motiv_Get,
			    Strength_Ret,
			    Sign_correction_Ret,
			    Modify_strength_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Null out charge of ~A"),
			    u_emot
			  ],
			  Roman_nl_Ret25),
	get_var(LEnv, u_emot, Emot_Get14),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get13),
	f_u_strength(u_emot, Strength_Ret26),
	f_u_modify_strength(Xx_reality_xx_Get13,
			    Emot_Get14,
			    Strength_Ret26,
			    -1.0,
			    Modify_strength_Ret27),
	f_u_print_tasks(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_divert_emot_to_tlg, classof, claz_function),
   set_opv(u_divert_emot_to_tlg, compile_as, kw_function),
   set_opv(u_divert_emot_to_tlg, function, f_u_divert_emot_to_tlg),
   DefunResult=u_divert_emot_to_tlg.
/*
:- side_effect(assert_lsp(u_divert_emot_to_tlg,
			  wl:lambda_def(defun, u_divert_emot_to_tlg, f_u_divert_emot_to_tlg, [u_emot, u_context, u_top_level_goal], [[let, [[u_main_motiv, [u_ob_c36_get, u_top_level_goal, [quote, u_main_motiv]]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Divert strength of ~A to ~A"), u_emot, u_top_level_goal], [u_modify_strength, u_xx_reality_xx, u_main_motiv, [u_strength, u_emot], [u_sign_correction, u_emot, u_main_motiv]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Null out charge of ~A"), u_emot], [u_modify_strength, u_xx_reality_xx, u_emot, [u_strength, u_emot], -1.0], [u_print_tasks]]]))).
*/
/*
:- side_effect(assert_lsp(u_divert_emot_to_tlg,
			  wl:arglist_info(u_divert_emot_to_tlg, f_u_divert_emot_to_tlg, [u_emot, u_context, u_top_level_goal], arginfo{all:[u_emot, u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot, u_context, u_top_level_goal], opt:0, req:[u_emot, u_context, u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_divert_emot_to_tlg,
			  wl:init_args(exact_only, f_u_divert_emot_to_tlg))).
*/
/*
(defun sign-correction (emot1 emot2)
  (cond
   ((or (and (ty$instance? emot1 'neg-emotion)
             (ty$instance? emot2 'pos-emotion))
        (and (ty$instance? emot1 'pos-emotion)
             (ty$instance? emot2 'neg-emotion)))
    -1.0)
   (else 1.0)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21116 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'sign-correction',[emot1,emot2],[cond,[[or,[and,['ty$instance?',emot1,[quote,'neg-emotion']],['ty$instance?',emot2,[quote,'pos-emotion']]],[and,['ty$instance?',emot1,[quote,'pos-emotion']],['ty$instance?',emot2,[quote,'neg-emotion']]]],-1.0],[else,1.0]]])
wl:lambda_def(defun, u_sign_correction, f_u_sign_correction, [u_emot1, u_emot2], [[cond, [[or, [and, [u_ty_c36_instance_c63, u_emot1, [quote, u_neg_emotion]], [u_ty_c36_instance_c63, u_emot2, [quote, u_pos_emotion]]], [and, [u_ty_c36_instance_c63, u_emot1, [quote, u_pos_emotion]], [u_ty_c36_instance_c63, u_emot2, [quote, u_neg_emotion]]]], -1.0], [u_else, 1.0]]]).
wl:arglist_info(u_sign_correction, f_u_sign_correction, [u_emot1, u_emot2], arginfo{all:[u_emot1, u_emot2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot1, u_emot2], opt:0, req:[u_emot1, u_emot2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_sign_correction).

/*

### Compiled:  `U::SIGN-CORRECTION` 
*/
f_u_sign_correction(Emot1, Emot2, ElseResult21) :-
	nop(global_env(Env)),
	Env24=[bv(u_emot1, Emot1), bv(u_emot2, Emot2)|Env],
	(   get_var(Env24, u_emot1, Emot1_Get),
	    f_u_ty_c36_instance_c63(Emot1_Get, u_neg_emotion, IFTEST6),
	    (   IFTEST6\==[]
	    ->  get_var(Env24, u_emot2, Emot2_Get),
		f_u_ty_c36_instance_c63(Emot2_Get, u_pos_emotion, TrueResult),
		FORM1_Res=TrueResult
	    ;   FORM1_Res=[]
	    ),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(Env24, u_emot1, Emot1_Get13),
	    f_u_ty_c36_instance_c63(Emot1_Get13, u_pos_emotion, IFTEST11),
	    (   IFTEST11\==[]
	    ->  get_var(Env24, u_emot2, Emot2_Get14),
		f_u_ty_c36_instance_c63(Emot2_Get14,
					u_neg_emotion,
					TrueResult15),
		IFTEST=TrueResult15
	    ;   IFTEST=[]
	    )
	),
	(   IFTEST\==[]
	->  ElseResult21= -1.0
	;   get_var(Env24, u_else, IFTEST17),
	    (   IFTEST17\==[]
	    ->  ElseResult21=1.0
	    ;   ElseResult21=[]
	    )
	).
:- set_opv(f_u_sign_correction, classof, claz_function),
   set_opv(u_sign_correction, compile_as, kw_function),
   set_opv(u_sign_correction, function, f_u_sign_correction),
   DefunResult=u_sign_correction.
/*
:- side_effect(assert_lsp(u_sign_correction,
			  wl:lambda_def(defun, u_sign_correction, f_u_sign_correction, [u_emot1, u_emot2], [[cond, [[or, [and, [u_ty_c36_instance_c63, u_emot1, [quote, u_neg_emotion]], [u_ty_c36_instance_c63, u_emot2, [quote, u_pos_emotion]]], [and, [u_ty_c36_instance_c63, u_emot1, [quote, u_pos_emotion]], [u_ty_c36_instance_c63, u_emot2, [quote, u_neg_emotion]]]], -1.0], [u_else, 1.0]]]))).
*/
/*
:- side_effect(assert_lsp(u_sign_correction,
			  wl:arglist_info(u_sign_correction, f_u_sign_correction, [u_emot1, u_emot2], arginfo{all:[u_emot1, u_emot2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot1, u_emot2], opt:0, req:[u_emot1, u_emot2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_sign_correction,
			  wl:init_args(exact_only, f_u_sign_correction))).
*/
/*
(defun emotion-sign (emot)
  (if (ty$instance? emot 'neg-emotion)
      -1.0
      1.0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21379 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'emotion-sign',[emot],[if,['ty$instance?',emot,[quote,'neg-emotion']],-1.0,1.0]])
wl:lambda_def(defun, u_emotion_sign, f_u_emotion_sign, [u_emot], [[if, [u_ty_c36_instance_c63, u_emot, [quote, u_neg_emotion]], -1.0, 1.0]]).
wl:arglist_info(u_emotion_sign, f_u_emotion_sign, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_emotion_sign).

/*

### Compiled:  `U::EMOTION-SIGN` 
*/
f_u_emotion_sign(Emot, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_emot, Emot)|Env],
	get_var(Env9, u_emot, Emot_Get),
	f_u_ty_c36_instance_c63(Emot_Get, u_neg_emotion, IFTEST),
	(   IFTEST\==[]
	->  FnResult= -1.0
	;   FnResult=1.0
	).
:- set_opv(f_u_emotion_sign, classof, claz_function),
   set_opv(u_emotion_sign, compile_as, kw_function),
   set_opv(u_emotion_sign, function, f_u_emotion_sign),
   DefunResult=u_emotion_sign.
/*
:- side_effect(assert_lsp(u_emotion_sign,
			  wl:lambda_def(defun, u_emotion_sign, f_u_emotion_sign, [u_emot], [[if, [u_ty_c36_instance_c63, u_emot, [quote, u_neg_emotion]], -1.0, 1.0]]))).
*/
/*
:- side_effect(assert_lsp(u_emotion_sign,
			  wl:arglist_info(u_emotion_sign, f_u_emotion_sign, [u_emot], arginfo{all:[u_emot], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_emot], opt:0, req:[u_emot], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_emotion_sign,
			  wl:init_args(exact_only, f_u_emotion_sign))).
*/
/*
(defun backtrack-top-level-goal (top-level-goal next-context)
  (ndbg-roman-nl *gate-dbg* rule
                 "Attempting to backtrack for top-level goal "(defun backtrack-top-level-goal (top-level-goal next-context)\n  (ndbg-roman-nl *gate-dbg* rule\n                 \"Attempting to backtrack for top-level goal ~A in ~A\"\n                 top-level-goal next-context)\n  (yloop (initial (backtrack-wall (get-backtrack-wall top-level-goal))\n                 (sprouts nil)\n                 (done? nil))\n        (yuntil done?)\n        (ydo (if (eq? backtrack-wall next-context)\n                (progn\n                 (ndbg-roman *gate-dbg* rule \"Top-level goal\")\n                 (ndbg-roman *gate-dbg* rule \" ~A\" top-level-goal)\n                 (ndbg-roman-nl *gate-dbg* rule\n                                \" fails: all possibilities exhausted\")\n                 ; The below will do for now.\n                 (all-possibilities-failed top-level-goal backtrack-wall)\n                 ; If return value is NIL, there were no successful\n                 ;  mutations and so the top-level goal is terminated.\n                 ; If return value is T, there was a successful mutation\n                 ;  and the next-context of this top-level-goal has\n                 ;  already been set by that process.\n                 ; Was (setq done? (null? returned-next-context)) because this\n                 ; algorithm had to do some more backup in the old mutation alg.\n                 (setq done? t))\n                (progn\n                 (setq sprouts\n                      (prune-possibilities (cx$children\n                                            (cx$parent next-context))))\n                 (if sprouts\n                     (progn\n                      (setq next-context (car sprouts)) ; was random-element\n                      ; but we want to go by ordering, so no random...\n                      (set-next-context top-level-goal next-context)\n                      (ndbg-roman *gate-dbg* rule \"Backtracking\")\n                      (ndbg-roman *gate-dbg* rule\n                            \" to next context of ~A for ~A\"\n                            next-context top-level-goal)\n                      (ndbg-newline *gate-dbg* rule)\n                      (setq done? t))\n                     (setq next-context (cx$parent next-context))))))))\n\n;\n; Currently, DAYDREAMER stops upon the first success and so the below\n; function is invoked if there was not a single success.\n;\n; Returns leaf if should continue because more contexts have been\n; sprouted, or NIL if we should stop.\n;\n; Todo: What about eventual mutation exhaustion?\n;\n; Possibly, a top-level goal failure has been asserted in some context;\n; But we have to do this in the 'resolution context', which in this\n; case is backtrack-wall.\n;\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'backtrack-top-level-goal',['top-level-goal','next-context'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Attempting to backtrack for top-level goal ~A in ~A"),'top-level-goal','next-context'],[yloop,[initial,['backtrack-wall',['get-backtrack-wall','top-level-goal']],[sprouts,[]],['done?',[]]],[yuntil,'done?'],[ydo,[if,['eq?','backtrack-wall','next-context'],[progn,['ndbg-roman','*gate-dbg*',rule,'$STRING'("Top-level goal")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A"),'top-level-goal'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'(" fails: all possibilities exhausted")],['all-possibilities-failed','top-level-goal','backtrack-wall'],[setq,'done?',t]],[progn,[setq,sprouts,['prune-possibilities',['cx$children',['cx$parent','next-context']]]],[if,sprouts,[progn,[setq,'next-context',[car,sprouts]],['set-next-context','top-level-goal','next-context'],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Backtracking")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" to next context of ~A for ~A"),'next-context','top-level-goal'],['ndbg-newline','*gate-dbg*',rule],[setq,'done?',t]],[setq,'next-context',['cx$parent','next-context']]]]]]]])
wl:lambda_def(defun, u_backtrack_top_level_goal, f_u_backtrack_top_level_goal, [u_top_level_goal, u_next_context], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Attempting to backtrack for top-level goal ~A in ~A"), u_top_level_goal, u_next_context], [u_yloop, [u_initial, [u_backtrack_wall, [u_get_backtrack_wall, u_top_level_goal]], [u_sprouts, []], [u_done_c63, []]], [u_yuntil, u_done_c63], [u_ydo, [if, [u_eq_c63, u_backtrack_wall, u_next_context], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " fails: all possibilities exhausted")], [u_all_possibilities_failed, u_top_level_goal, u_backtrack_wall], [setq, u_done_c63, t]], [progn, [setq, u_sprouts, [u_prune_possibilities, [u_cx_c36_children, [u_cx_c36_parent, u_next_context]]]], [if, u_sprouts, [progn, [setq, u_next_context, [car, u_sprouts]], [u_set_next_context, u_top_level_goal, u_next_context], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Backtracking")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " to next context of ~A for ~A"), u_next_context, u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [setq, u_done_c63, t]], [setq, u_next_context, [u_cx_c36_parent, u_next_context]]]]]]]]).
wl:arglist_info(u_backtrack_top_level_goal, f_u_backtrack_top_level_goal, [u_top_level_goal, u_next_context], arginfo{all:[u_top_level_goal, u_next_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_next_context], opt:0, req:[u_top_level_goal, u_next_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_backtrack_top_level_goal).

/*

### Compiled:  `U::BACKTRACK-TOP-LEVEL-GOAL` 
*/
f_u_backtrack_top_level_goal(Top_level_goal, Next_context, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_top_level_goal, Top_level_goal), bv(u_next_context, Next_context)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Attempting to backtrack for top-level goal ~A in ~A"),
			    u_top_level_goal,
			    u_next_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_backtrack_wall,
			[u_get_backtrack_wall, u_top_level_goal]
		      ],
		      [u_sprouts, []],
		      [u_done_c63, []]
		    ],
		    [u_yuntil, u_done_c63],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_backtrack_wall, u_next_context],
			
			[ progn,
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*], claz_base_character, "Top-level goal")
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*], claz_base_character, " ~A"),
			    u_top_level_goal
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     " fails: all possibilities exhausted")
			  ],
			  
			  [ u_all_possibilities_failed,
			    u_top_level_goal,
			    u_backtrack_wall
			  ],
			  [setq, u_done_c63, t]
			],
			
			[ progn,
			  
			  [ setq,
			    u_sprouts,
			    
			    [ u_prune_possibilities,
			      
			      [ u_cx_c36_children,
				[u_cx_c36_parent, u_next_context]
			      ]
			    ]
			  ],
			  
			  [ if,
			    u_sprouts,
			    
			    [ progn,
			      [setq, u_next_context, [car, u_sprouts]],
			      
			      [ u_set_next_context,
				u_top_level_goal,
				u_next_context
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 "Backtracking")
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 " to next context of ~A for ~A"),
				u_next_context,
				u_top_level_goal
			      ],
			      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			      [setq, u_done_c63, t]
			    ],
			    
			    [ setq,
			      u_next_context,
			      [u_cx_c36_parent, u_next_context]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_backtrack_top_level_goal, classof, claz_function),
   set_opv(u_backtrack_top_level_goal, compile_as, kw_function),
   set_opv(u_backtrack_top_level_goal, function, f_u_backtrack_top_level_goal),
   DefunResult=u_backtrack_top_level_goal.
/*
:- side_effect(assert_lsp(u_backtrack_top_level_goal,
			  wl:lambda_def(defun, u_backtrack_top_level_goal, f_u_backtrack_top_level_goal, [u_top_level_goal, u_next_context], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Attempting to backtrack for top-level goal ~A in ~A"), u_top_level_goal, u_next_context], [u_yloop, [u_initial, [u_backtrack_wall, [u_get_backtrack_wall, u_top_level_goal]], [u_sprouts, []], [u_done_c63, []]], [u_yuntil, u_done_c63], [u_ydo, [if, [u_eq_c63, u_backtrack_wall, u_next_context], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A"), u_top_level_goal], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " fails: all possibilities exhausted")], [u_all_possibilities_failed, u_top_level_goal, u_backtrack_wall], [setq, u_done_c63, t]], [progn, [setq, u_sprouts, [u_prune_possibilities, [u_cx_c36_children, [u_cx_c36_parent, u_next_context]]]], [if, u_sprouts, [progn, [setq, u_next_context, [car, u_sprouts]], [u_set_next_context, u_top_level_goal, u_next_context], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Backtracking")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " to next context of ~A for ~A"), u_next_context, u_top_level_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [setq, u_done_c63, t]], [setq, u_next_context, [u_cx_c36_parent, u_next_context]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_backtrack_top_level_goal,
			  wl:arglist_info(u_backtrack_top_level_goal, f_u_backtrack_top_level_goal, [u_top_level_goal, u_next_context], arginfo{all:[u_top_level_goal, u_next_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_next_context], opt:0, req:[u_top_level_goal, u_next_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_backtrack_top_level_goal,
			  wl:init_args(exact_only, f_u_backtrack_top_level_goal))).
*/
/*
 The below will do for now.
*/
/*
 If return value is NIL, there were no successful
*/
/*
  mutations and so the top-level goal is terminated.
*/
/*
 If return value is T, there was a successful mutation
*/
/*
  and the next-context of this top-level-goal has
*/
/*
  already been set by that process.
*/
/*
 Was (setq done? (null? returned-next-context)) because this
*/
/*
 algorithm had to do some more backup in the old mutation alg.
*/
/*
 was random-element
*/
/*
 but we want to go by ordering, so no random...
*/
/*
*/
/*
 Currently, DAYDREAMER stops upon the first success and so the below
*/
/*
 function is invoked if there was not a single success.
*/
/*
*/
/*
 Returns leaf if should continue because more contexts have been
*/
/*
 sprouted, or NIL if we should stop.
*/
/*
*/
/*
 Todo: What about eventual mutation exhaustion?
*/
/*
*/
/*
 Possibly, a top-level goal failure has been asserted in some context;
*/
/*
 But we have to do this in the 'resolution context', which in this
*/
/*
 case is backtrack-wall.
*/
/*
*/
/*
(defun all-possibilities-failed (top-level-goal backtrack-wall)
 (ndbg-roman-nl *gate-dbg* rule "All possibilities failed for "(defun all-possibilities-failed (top-level-goal backtrack-wall)\n (ndbg-roman-nl *gate-dbg* rule \"All possibilities failed for ~A in ~A\"\n                           top-level-goal backtrack-wall)\n (let ((result nil))\n  (if (and (daydreaming-mode?)\n           (eq? 'imaginary (ob$get top-level-goal 'planning-type))\n           (setq result (action-mutations top-level-goal backtrack-wall)))\n      result\n;      (let ((failed-goal (ob$fcreate `(FAILED-GOAL obj ,(ob$get\n;                                                           top-level-goal\n;                                                           'obj))))))\n      (progn\n;       (if (interrogate \"Break? (before tlg failure) \") (breakpoint))\n       (terminate-top-level-goal top-level-goal\n                                (make-goal-failure top-level-goal backtrack-wall\n                                                   nil *me-belief-path*\n                                                   top-level-goal t)\n                                backtrack-wall)\n       nil))))\n\n; This is pruning possibilities after they are generated. Another way\n; is to prune before they are generated.\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'all-possibilities-failed',['top-level-goal','backtrack-wall'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("All possibilities failed for ~A in ~A"),'top-level-goal','backtrack-wall'],[let,[[result,[]]],[if,[and,['daydreaming-mode?'],['eq?',[quote,imaginary],['ob$get','top-level-goal',[quote,'planning-type']]],[setq,result,['action-mutations','top-level-goal','backtrack-wall']]],result,[progn,['terminate-top-level-goal','top-level-goal',['make-goal-failure','top-level-goal','backtrack-wall',[],'*me-belief-path*','top-level-goal',t],'backtrack-wall'],[]]]]])
wl:lambda_def(defun, u_all_possibilities_failed, f_u_all_possibilities_failed, [u_top_level_goal, u_backtrack_wall], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "All possibilities failed for ~A in ~A"), u_top_level_goal, u_backtrack_wall], [let, [[u_result, []]], [if, [and, [u_daydreaming_mode_c63], [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [setq, u_result, [u_action_mutations, u_top_level_goal, u_backtrack_wall]]], u_result, [progn, [u_terminate_top_level_goal, u_top_level_goal, [u_make_goal_failure, u_top_level_goal, u_backtrack_wall, [], u_xx_me_belief_path_xx, u_top_level_goal, t], u_backtrack_wall], []]]]]).
wl:arglist_info(u_all_possibilities_failed, f_u_all_possibilities_failed, [u_top_level_goal, u_backtrack_wall], arginfo{all:[u_top_level_goal, u_backtrack_wall], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_backtrack_wall], opt:0, req:[u_top_level_goal, u_backtrack_wall], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_all_possibilities_failed).

/*

### Compiled:  `U::ALL-POSSIBILITIES-FAILED` 
*/
f_u_all_possibilities_failed(Top_level_goal, Backtrack_wall, FnResult) :-
	nop(global_env(Env)),
	Env28=[bv(u_top_level_goal, Top_level_goal), bv(u_backtrack_wall, Backtrack_wall)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "All possibilities failed for ~A in ~A"),
			    u_top_level_goal,
			    u_backtrack_wall
			  ],
			  Roman_nl_Ret),
	LEnv=[bv(u_result, [])|Env28],
	f_u_daydreaming_mode_c63(IFTEST9),
	(   IFTEST9\==[]
	->  f_u_eq_c63([quote, u_imaginary],
		       [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		       IFTEST11),
	    (   IFTEST11\==[]
	    ->  get_var(LEnv, u_backtrack_wall, Backtrack_wall_Get),
		get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
		f_u_action_mutations(Top_level_goal_Get,
				     Backtrack_wall_Get,
				     TrueResult),
		set_var(LEnv, u_result, TrueResult),
		IFTEST=TrueResult
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_result, Result_Get),
	    FnResult=Result_Get
	;   get_var(LEnv, u_backtrack_wall, Backtrack_wall_Get21),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get19),
	    get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_make_goal_failure(Top_level_goal_Get19,
				  Backtrack_wall_Get21,
				  [],
				  Xx_me_belief_path_xx_Get,
				  Top_level_goal_Get19,
				  t,
				  T),
	    get_var(LEnv, u_backtrack_wall, Backtrack_wall_Get24),
	    f_u_terminate_top_level_goal(Top_level_goal_Get19,
					 T,
					 Backtrack_wall_Get24,
					 Level_goal_Ret),
	    FnResult=[]
	).
:- set_opv(f_u_all_possibilities_failed, classof, claz_function),
   set_opv(u_all_possibilities_failed, compile_as, kw_function),
   set_opv(u_all_possibilities_failed, function, f_u_all_possibilities_failed),
   DefunResult=u_all_possibilities_failed.
/*
:- side_effect(assert_lsp(u_all_possibilities_failed,
			  wl:lambda_def(defun, u_all_possibilities_failed, f_u_all_possibilities_failed, [u_top_level_goal, u_backtrack_wall], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "All possibilities failed for ~A in ~A"), u_top_level_goal, u_backtrack_wall], [let, [[u_result, []]], [if, [and, [u_daydreaming_mode_c63], [u_eq_c63, [quote, u_imaginary], [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]], [setq, u_result, [u_action_mutations, u_top_level_goal, u_backtrack_wall]]], u_result, [progn, [u_terminate_top_level_goal, u_top_level_goal, [u_make_goal_failure, u_top_level_goal, u_backtrack_wall, [], u_xx_me_belief_path_xx, u_top_level_goal, t], u_backtrack_wall], []]]]]))).
*/
/*
:- side_effect(assert_lsp(u_all_possibilities_failed,
			  wl:arglist_info(u_all_possibilities_failed, f_u_all_possibilities_failed, [u_top_level_goal, u_backtrack_wall], arginfo{all:[u_top_level_goal, u_backtrack_wall], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_backtrack_wall], opt:0, req:[u_top_level_goal, u_backtrack_wall], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_all_possibilities_failed,
			  wl:init_args(exact_only, f_u_all_possibilities_failed))).
*/
/*
      (let ((failed-goal (ob$fcreate `(FAILED-GOAL obj ,(ob$get
*/
/*
                                                           top-level-goal
*/
/*
                                                           'obj))))))
*/
/*
       (if (interrogate "Break? (before tlg failure) ") (breakpoint))
*/
/*
 This is pruning possibilities after they are generated. Another way
*/
/*
 is to prune before they are generated.
*/
/*
(defun prune-possibilities (contexts)
  (ndbg-roman-nl *gate-dbg* rule "Pruning possibilities from "(defun prune-possibilities (contexts)\n  (ndbg-roman-nl *gate-dbg* rule \"Pruning possibilities from ~A\" contexts)\n  (yloop\n   (initial (result nil))\n   (yfor context in contexts)\n   (ydo\n    (if (and (not (ob$get context 'rules-run?))\n             (not (ob$get context 'dd-goal-sprout?)))\n        ; will the above prevent backups to the first dd goal context?\n        (setq result (cons context result))))\n   (yresult (sort result (lambda (context1 context2)\n                                (> (ordering context1)\n                                   (ordering context2)))))))\n\n; Extra slots associated with a top-level goal:\n;       status: 'runable (if this goal is ready to run)\n;               'halted (if this goal is halted)\n;               'waiting (if this goal is waiting to be performed)\n;       active-goal: used for the top-level goal upon replacement\n;       planning-type: 'real or 'imaginary\n;       backtrack-wall: (only if planning-type = 'imaginary) backtrack\n;                       wall context\n;       next-context: (only if planning-type = 'imaginary) next context\n;                     to run\n;       mutation-plan-contexts: slot values contain ideas for new plans\n;       run-mutations?: t or nil\n;       termination-context: not set until the top-level goal is terminated\n;        main-motiv: main motivation emotion\n;\n;       For top-level goals AND subgoals:\n;       activation-context: points to the context in which the goal\n;              was first activated.\n;       top-level-goal: points to the top-level goal for all subgoals\n;               and, of course, the top-level goal itself\n;\n; Termination and activation contexts are more or less at this time\n; for REVERSAL. (But, see also uses of 'top-context which really\n; are referring to 'activation-context)\n;\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'prune-possibilities',[contexts],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Pruning possibilities from ~A"),contexts],[yloop,[initial,[result,[]]],[yfor,context,in,contexts],[ydo,[if,[and,[not,['ob$get',context,[quote,'rules-run?']]],[not,['ob$get',context,[quote,'dd-goal-sprout?']]]],[setq,result,[cons,context,result]]]],[yresult,[sort,result,[lambda,[context1,context2],[>,[ordering,context1],[ordering,context2]]]]]]])
wl:lambda_def(defun, u_prune_possibilities, f_u_prune_possibilities, [u_contexts], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Pruning possibilities from ~A"), u_contexts], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_context, u_in, u_contexts], [u_ydo, [if, [and, [not, [u_ob_c36_get, u_context, [quote, u_rules_run_c63]]], [not, [u_ob_c36_get, u_context, [quote, u_dd_goal_sprout_c63]]]], [setq, u_result, [cons, u_context, u_result]]]], [u_yresult, [sort, u_result, [lambda, [u_context1, u_context2], [>, [u_ordering, u_context1], [u_ordering, u_context2]]]]]]]).
wl:arglist_info(u_prune_possibilities, f_u_prune_possibilities, [u_contexts], arginfo{all:[u_contexts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_contexts], opt:0, req:[u_contexts], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_prune_possibilities).

/*

### Compiled:  `U::PRUNE-POSSIBILITIES` 
*/
f_u_prune_possibilities(Contexts, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_contexts, Contexts)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Pruning possibilities from ~A"),
			    u_contexts
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_context, u_in, u_contexts],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ not,
			    [u_ob_c36_get, u_context, [quote, u_rules_run_c63]]
			  ],
			  
			  [ not,
			    
			    [ u_ob_c36_get,
			      u_context,
			      [quote, u_dd_goal_sprout_c63]
			    ]
			  ]
			],
			[setq, u_result, [cons, u_context, u_result]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ sort,
			u_result,
			
			[ lambda,
			  [u_context1, u_context2],
			  [>, [u_ordering, u_context1], [u_ordering, u_context2]]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_prune_possibilities, classof, claz_function),
   set_opv(u_prune_possibilities, compile_as, kw_function),
   set_opv(u_prune_possibilities, function, f_u_prune_possibilities),
   DefunResult=u_prune_possibilities.
/*
:- side_effect(assert_lsp(u_prune_possibilities,
			  wl:lambda_def(defun, u_prune_possibilities, f_u_prune_possibilities, [u_contexts], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Pruning possibilities from ~A"), u_contexts], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_context, u_in, u_contexts], [u_ydo, [if, [and, [not, [u_ob_c36_get, u_context, [quote, u_rules_run_c63]]], [not, [u_ob_c36_get, u_context, [quote, u_dd_goal_sprout_c63]]]], [setq, u_result, [cons, u_context, u_result]]]], [u_yresult, [sort, u_result, [lambda, [u_context1, u_context2], [>, [u_ordering, u_context1], [u_ordering, u_context2]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_prune_possibilities,
			  wl:arglist_info(u_prune_possibilities, f_u_prune_possibilities, [u_contexts], arginfo{all:[u_contexts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_contexts], opt:0, req:[u_contexts], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_prune_possibilities,
			  wl:init_args(exact_only, f_u_prune_possibilities))).
*/
/*
 will the above prevent backups to the first dd goal context?
*/
/*
 Extra slots associated with a top-level goal:
*/
/*
       status: 'runable (if this goal is ready to run)
*/
/*
               'halted (if this goal is halted)
*/
/*
               'waiting (if this goal is waiting to be performed)
*/
/*
       active-goal: used for the top-level goal upon replacement
*/
/*
       planning-type: 'real or 'imaginary
*/
/*
       backtrack-wall: (only if planning-type = 'imaginary) backtrack
*/
/*
                       wall context
*/
/*
       next-context: (only if planning-type = 'imaginary) next context
*/
/*
                     to run
*/
/*
       mutation-plan-contexts: slot values contain ideas for new plans
*/
/*
       run-mutations?: t or nil
*/
/*
       termination-context: not set until the top-level goal is terminated
*/
/*
        main-motiv: main motivation emotion
*/
/*
*/
/*
       For top-level goals AND subgoals:
*/
/*
       activation-context: points to the context in which the goal
*/
/*
              was first activated.
*/
/*
       top-level-goal: points to the top-level goal for all subgoals
*/
/*
               and, of course, the top-level goal itself
*/
/*
*/
/*
 Termination and activation contexts are more or less at this time
*/
/*
 for REVERSAL. (But, see also uses of 'top-context which really
*/
/*
 are referring to 'activation-context)
*/
/*
*/
/*
(setq *genable-emot-thresh* 0.1)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27048 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*genable-emot-thresh*',0.1])
:- set_var(AEnv, setq, u_xx_genable_emot_thresh_xx, 0.1).
/*
(defun activate-top-level-goal (goal context bd rule)
  (ndbg-roman-nl *gate-dbg* rule "******************")
  (ndbg-roman *gate-dbg* rule "Activate top-level goal")
  (ndbg-roman *gate-dbg* rule " "(defun activate-top-level-goal (goal context bd rule)\n  (ndbg-roman-nl *gate-dbg* rule \"******************\")\n  (ndbg-roman *gate-dbg* rule \"Activate top-level goal\")\n  (ndbg-roman *gate-dbg* rule \" ~A in ~A\" goal context)\n  (ndbg-newline *gate-dbg* rule)\n  (let ((emotions (ob$gets rule 'emotion))\n        (ddg? (dd-goal? goal))\n        (new-context nil)\n        (any-emot nil)\n        (main-motiv nil))\n       (setq *top-level-goals* (cons goal *top-level-goals*))\n       (if ddg?\n           (no-gen (cx$assert context goal))\n           (cx$assert context goal))\n       (ob$add goal 'top-level-goal goal)\n       (if (null? (ob$get rule 'initial-status))\n           (ob$add goal 'status 'runable)\n           (ob$add goal 'status (ob$get rule 'initial-status)))\n       (yloop (yfor emotion in emotions)\n              (ydo (if (var? emotion)\n                       (setq main-motiv\n                        (setq emotion (ob$instantiate emotion bd)))\n                       (setq any-emot\n                        (setq emotion (ob$instantiate emotion bd))))\n                   (add-depend context emotion goal 1.0 0.0 0.0 nil)\n                   (emotion-add emotion)\n                   (if (or (eq? emotion main-motiv)\n                           (fl< (strength emotion) *genable-emot-thresh*))\n                       (no-gen (cx$assert context emotion))\n                       (cx$assert context emotion))))\n       ; should really batch strength recalculations above\n       (if (null? main-motiv)\n           (setq main-motiv any-emot))\n       (ob$add goal 'main-motiv main-motiv)\n       (if ddg?\n           (progn\n            (ob$add goal 'planning-type 'imaginary)\n            (setq new-context (cx$sprout context))\n            (ob$add new-context 'dd-goal-sprout? t)\n            (ob$add goal 'backtrack-wall new-context)\n            (ob$add goal 'activation-context new-context)\n            (ob$add goal 'next-context new-context))\n           (progn\n            (ob$add goal 'activation-context context)\n            (ob$add goal 'planning-type 'real)))\n       (print-tasks)))\n\n; ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27082 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'activate-top-level-goal',[goal,context,bd,rule],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("******************")],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Activate top-level goal")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A in ~A"),goal,context],['ndbg-newline','*gate-dbg*',rule],[let,[[emotions,['ob$gets',rule,[quote,emotion]]],['ddg?',['dd-goal?',goal]],['new-context',[]],['any-emot',[]],['main-motiv',[]]],[setq,'*top-level-goals*',[cons,goal,'*top-level-goals*']],[if,'ddg?',['no-gen',['cx$assert',context,goal]],['cx$assert',context,goal]],['ob$add',goal,[quote,'top-level-goal'],goal],[if,['null?',['ob$get',rule,[quote,'initial-status']]],['ob$add',goal,[quote,status],[quote,runable]],['ob$add',goal,[quote,status],['ob$get',rule,[quote,'initial-status']]]],[yloop,[yfor,emotion,in,emotions],[ydo,[if,['var?',emotion],[setq,'main-motiv',[setq,emotion,['ob$instantiate',emotion,bd]]],[setq,'any-emot',[setq,emotion,['ob$instantiate',emotion,bd]]]],['add-depend',context,emotion,goal,1.0,0.0,0.0,[]],['emotion-add',emotion],[if,[or,['eq?',emotion,'main-motiv'],['fl<',[strength,emotion],'*genable-emot-thresh*']],['no-gen',['cx$assert',context,emotion]],['cx$assert',context,emotion]]]],[if,['null?','main-motiv'],[setq,'main-motiv','any-emot']],['ob$add',goal,[quote,'main-motiv'],'main-motiv'],[if,'ddg?',[progn,['ob$add',goal,[quote,'planning-type'],[quote,imaginary]],[setq,'new-context',['cx$sprout',context]],['ob$add','new-context',[quote,'dd-goal-sprout?'],t],['ob$add',goal,[quote,'backtrack-wall'],'new-context'],['ob$add',goal,[quote,'activation-context'],'new-context'],['ob$add',goal,[quote,'next-context'],'new-context']],[progn,['ob$add',goal,[quote,'activation-context'],context],['ob$add',goal,[quote,'planning-type'],[quote,real]]]],['print-tasks']]])
wl:lambda_def(defun, u_activate_top_level_goal, f_u_activate_top_level_goal, [u_goal, u_context, u_bd, u_rule], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "******************")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Activate top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A in ~A"), u_goal, u_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [let, [[u_emotions, [u_ob_c36_gets, u_rule, [quote, u_emotion]]], [u_ddg_c63, [u_dd_goal_c63, u_goal]], [u_new_context, []], [u_any_emot, []], [u_main_motiv, []]], [setq, u_xx_top_level_goals_xx, [cons, u_goal, u_xx_top_level_goals_xx]], [if, u_ddg_c63, [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], [u_cx_c36_assert, u_context, u_goal]], [u_ob_c36_add, u_goal, [quote, u_top_level_goal], u_goal], [if, [u_null_c63, [u_ob_c36_get, u_rule, [quote, u_initial_status]]], [u_ob_c36_add, u_goal, [quote, u_status], [quote, u_runable]], [u_ob_c36_add, u_goal, [quote, u_status], [u_ob_c36_get, u_rule, [quote, u_initial_status]]]], [u_yloop, [u_yfor, u_emotion, u_in, u_emotions], [u_ydo, [if, [u_var_c63, u_emotion], [setq, u_main_motiv, [setq, u_emotion, [u_ob_c36_instantiate, u_emotion, u_bd]]], [setq, u_any_emot, [setq, u_emotion, [u_ob_c36_instantiate, u_emotion, u_bd]]]], [u_add_depend, u_context, u_emotion, u_goal, 1.0, 0.0, 0.0, []], [u_emotion_add, u_emotion], [if, [or, [u_eq_c63, u_emotion, u_main_motiv], [u_fl_c60, [u_strength, u_emotion], u_xx_genable_emot_thresh_xx]], [u_no_gen, [u_cx_c36_assert, u_context, u_emotion]], [u_cx_c36_assert, u_context, u_emotion]]]], [if, [u_null_c63, u_main_motiv], [setq, u_main_motiv, u_any_emot]], [u_ob_c36_add, u_goal, [quote, u_main_motiv], u_main_motiv], [if, u_ddg_c63, [progn, [u_ob_c36_add, u_goal, [quote, u_planning_type], [quote, u_imaginary]], [setq, u_new_context, [u_cx_c36_sprout, u_context]], [u_ob_c36_add, u_new_context, [quote, u_dd_goal_sprout_c63], t], [u_ob_c36_add, u_goal, [quote, u_backtrack_wall], u_new_context], [u_ob_c36_add, u_goal, [quote, u_activation_context], u_new_context], [u_ob_c36_add, u_goal, [quote, u_next_context], u_new_context]], [progn, [u_ob_c36_add, u_goal, [quote, u_activation_context], u_context], [u_ob_c36_add, u_goal, [quote, u_planning_type], [quote, real]]]], [u_print_tasks]]]).
wl:arglist_info(u_activate_top_level_goal, f_u_activate_top_level_goal, [u_goal, u_context, u_bd, u_rule], arginfo{all:[u_goal, u_context, u_bd, u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_bd, u_rule], opt:0, req:[u_goal, u_context, u_bd, u_rule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_activate_top_level_goal).

/*

### Compiled:  `U::ACTIVATE-TOP-LEVEL-GOAL` 
*/
f_u_activate_top_level_goal(Goal, Context, Bd, Rule, FnResult) :-
	nop(global_env(Env)),
	Env55=[bv(u_goal, Goal), bv(u_context, Context), bv(u_bd, Bd), bv(u_rule, Rule)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "******************")
			  ],
			  Roman_nl_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  "Activate top-level goal")
		       ],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*], claz_base_character, " ~A in ~A"),
			 u_goal,
			 u_context
		       ],
		       Ndbg_roman_Ret68),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule60),
	get_var(Env55, u_rule, Rule_Get),
	f_u_ob_c36_gets(Rule_Get, u_emotion, Emotions_Init),
	get_var(Env55, u_goal, Goal_Get),
	f_u_dd_goal_c63(Goal_Get, Ddg_c63_Init),
	LEnv=[bv(u_emotions, Emotions_Init), bv(u_ddg_c63, Ddg_c63_Init), bv(u_new_context, []), bv(u_any_emot, []), bv(u_main_motiv, [])|Env55],
	get_var(LEnv, u_goal, Goal_Get12),
	get_var(LEnv, u_xx_top_level_goals_xx, Xx_top_level_goals_xx_Get),
	Xx_top_level_goals_xx=[Goal_Get12|Xx_top_level_goals_xx_Get],
	set_var(LEnv, u_xx_top_level_goals_xx, Xx_top_level_goals_xx),
	get_var(LEnv, u_ddg_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], TrueResult),
	    _196710710=TrueResult
	;   get_var(LEnv, u_context, Context_Get),
	    get_var(LEnv, u_goal, Goal_Get18),
	    f_u_cx_c36_assert(Context_Get, Goal_Get18, ElseResult),
	    _196710710=ElseResult
	),
	get_var(LEnv, u_goal, Goal_Get21),
	f_u_ob_c36_add(Goal_Get21, u_top_level_goal, Goal_Get21, C36_add_Ret),
	f_u_null_c63([u_ob_c36_get, u_rule, [quote, u_initial_status]], IFTEST23),
	(   IFTEST23\==[]
	->  get_var(LEnv, u_goal, Goal_Get25),
	    f_u_ob_c36_add(Goal_Get25, u_status, u_runable, TrueResult28),
	    _196735442=TrueResult28
	;   get_var(LEnv, u_goal, Goal_Get26),
	    get_var(LEnv, u_rule, Rule_Get27),
	    f_u_ob_c36_get(Rule_Get27, u_initial_status, Initial_status),
	    f_u_ob_c36_add(Goal_Get26, u_status, Initial_status, ElseResult29),
	    _196735442=ElseResult29
	),
	f_u_yloop(
		  [ [u_yfor, u_emotion, u_in, u_emotions],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_var_c63, u_emotion],
			
			[ setq,
			  u_main_motiv,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			],
			
			[ setq,
			  u_any_emot,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			]
		      ],
		      [u_add_depend, u_context, u_emotion, u_goal, 1.0, 0.0, 0.0, []],
		      [u_emotion_add, u_emotion],
		      
		      [ if,
			
			[ or,
			  [u_eq_c63, u_emotion, u_main_motiv],
			  
			  [ u_fl_c60,
			    [u_strength, u_emotion],
			    u_xx_genable_emot_thresh_xx
			  ]
			],
			[u_no_gen, [u_cx_c36_assert, u_context, u_emotion]],
			[u_cx_c36_assert, u_context, u_emotion]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_null_c63(u_main_motiv, IFTEST30),
	(   IFTEST30\==[]
	->  get_var(LEnv, u_any_emot, Any_emot_Get),
	    set_var(LEnv, u_main_motiv, Any_emot_Get),
	    _196751962=Any_emot_Get
	;   _196751962=[]
	),
	get_var(LEnv, u_goal, Goal_Get34),
	get_var(LEnv, u_main_motiv, Main_motiv_Get),
	f_u_ob_c36_add(Goal_Get34, u_main_motiv, Main_motiv_Get, C36_add_Ret71),
	get_var(LEnv, u_ddg_c63, IFTEST36),
	(   IFTEST36\==[]
	->  get_var(LEnv, u_goal, Goal_Get39),
	    f_u_ob_c36_add(Goal_Get39, u_planning_type, u_imaginary, Imaginary),
	    get_var(LEnv, u_context, Context_Get40),
	    f_u_cx_c36_sprout(Context_Get40, New_context),
	    set_var(LEnv, u_new_context, New_context),
	    get_var(LEnv, u_new_context, New_context_Get),
	    f_u_ob_c36_add(New_context_Get, u_dd_goal_sprout_c63, t, T),
	    get_var(LEnv, u_goal, Goal_Get42),
	    get_var(LEnv, u_new_context, New_context_Get43),
	    f_u_ob_c36_add(Goal_Get42,
			   u_backtrack_wall,
			   New_context_Get43,
			   C36_add_Ret72),
	    get_var(LEnv, u_goal, Goal_Get44),
	    get_var(LEnv, u_new_context, New_context_Get45),
	    f_u_ob_c36_add(Goal_Get44,
			   u_activation_context,
			   New_context_Get45,
			   C36_add_Ret73),
	    get_var(LEnv, u_goal, Goal_Get46),
	    get_var(LEnv, u_new_context, New_context_Get47),
	    f_u_ob_c36_add(Goal_Get46,
			   u_next_context,
			   New_context_Get47,
			   TrueResult51),
	    _196777350=TrueResult51
	;   get_var(LEnv, u_context, Context_Get49),
	    get_var(LEnv, u_goal, Goal_Get48),
	    f_u_ob_c36_add(Goal_Get48,
			   u_activation_context,
			   Context_Get49,
			   C36_add_Ret74),
	    get_var(LEnv, u_goal, Goal_Get50),
	    f_u_ob_c36_add(Goal_Get50, u_planning_type, real, ElseResult52),
	    _196777350=ElseResult52
	),
	f_u_print_tasks(LetResult),
	LetResult=FnResult.
:- set_opv(f_u_activate_top_level_goal, classof, claz_function),
   set_opv(u_activate_top_level_goal, compile_as, kw_function),
   set_opv(u_activate_top_level_goal, function, f_u_activate_top_level_goal),
   DefunResult=u_activate_top_level_goal.
/*
:- side_effect(assert_lsp(u_activate_top_level_goal,
			  wl:lambda_def(defun, u_activate_top_level_goal, f_u_activate_top_level_goal, [u_goal, u_context, u_bd, u_rule], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "******************")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Activate top-level goal")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A in ~A"), u_goal, u_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [let, [[u_emotions, [u_ob_c36_gets, u_rule, [quote, u_emotion]]], [u_ddg_c63, [u_dd_goal_c63, u_goal]], [u_new_context, []], [u_any_emot, []], [u_main_motiv, []]], [setq, u_xx_top_level_goals_xx, [cons, u_goal, u_xx_top_level_goals_xx]], [if, u_ddg_c63, [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], [u_cx_c36_assert, u_context, u_goal]], [u_ob_c36_add, u_goal, [quote, u_top_level_goal], u_goal], [if, [u_null_c63, [u_ob_c36_get, u_rule, [quote, u_initial_status]]], [u_ob_c36_add, u_goal, [quote, u_status], [quote, u_runable]], [u_ob_c36_add, u_goal, [quote, u_status], [u_ob_c36_get, u_rule, [quote, u_initial_status]]]], [u_yloop, [u_yfor, u_emotion, u_in, u_emotions], [u_ydo, [if, [u_var_c63, u_emotion], [setq, u_main_motiv, [setq, u_emotion, [u_ob_c36_instantiate, u_emotion, u_bd]]], [setq, u_any_emot, [setq, u_emotion, [u_ob_c36_instantiate, u_emotion, u_bd]]]], [u_add_depend, u_context, u_emotion, u_goal, 1.0, 0.0, 0.0, []], [u_emotion_add, u_emotion], [if, [or, [u_eq_c63, u_emotion, u_main_motiv], [u_fl_c60, [u_strength, u_emotion], u_xx_genable_emot_thresh_xx]], [u_no_gen, [u_cx_c36_assert, u_context, u_emotion]], [u_cx_c36_assert, u_context, u_emotion]]]], [if, [u_null_c63, u_main_motiv], [setq, u_main_motiv, u_any_emot]], [u_ob_c36_add, u_goal, [quote, u_main_motiv], u_main_motiv], [if, u_ddg_c63, [progn, [u_ob_c36_add, u_goal, [quote, u_planning_type], [quote, u_imaginary]], [setq, u_new_context, [u_cx_c36_sprout, u_context]], [u_ob_c36_add, u_new_context, [quote, u_dd_goal_sprout_c63], t], [u_ob_c36_add, u_goal, [quote, u_backtrack_wall], u_new_context], [u_ob_c36_add, u_goal, [quote, u_activation_context], u_new_context], [u_ob_c36_add, u_goal, [quote, u_next_context], u_new_context]], [progn, [u_ob_c36_add, u_goal, [quote, u_activation_context], u_context], [u_ob_c36_add, u_goal, [quote, u_planning_type], [quote, real]]]], [u_print_tasks]]]))).
*/
/*
:- side_effect(assert_lsp(u_activate_top_level_goal,
			  wl:arglist_info(u_activate_top_level_goal, f_u_activate_top_level_goal, [u_goal, u_context, u_bd, u_rule], arginfo{all:[u_goal, u_context, u_bd, u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_bd, u_rule], opt:0, req:[u_goal, u_context, u_bd, u_rule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_activate_top_level_goal,
			  wl:init_args(exact_only, f_u_activate_top_level_goal))).
*/
/*
 should really batch strength recalculations above
*/
/*
 End of file.
*/


%; Total compilation time: 20.723 seconds


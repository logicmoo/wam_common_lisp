#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "dd_gen" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
%; Start time: Sat Dec 23 21:23:53 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
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
 10/9/84:  Original generator written
*/
/*
  2/8/86:  Started adding new generation code
*/
/*
 9/24/86:  Took out flavor calls
*/
/*
 11/7/86:  Started adding some new entries
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *references* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:454 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*references*',[]])
:- set_var(AEnv, setq, u_xx_references_xx, []).
/*
(defun gn (con) (generate1 con *global-switches* *reality* *me-belief-path*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:480 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,gn,[con],[generate1,con,'*global-switches*','*reality*','*me-belief-path*']])
wl:lambda_def(defun, u_gn, f_u_gn, [u_con], [[u_generate1, u_con, u_xx_global_switches_xx, u_xx_reality_xx, u_xx_me_belief_path_xx]]).
wl:arglist_info(u_gn, f_u_gn, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gn).

/*

### Compiled:  `U::GN` 
*/
f_u_gn(Con, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_con, Con)|Env],
	get_var(Env10, u_con, Con_Get),
	get_var(Env10, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
	get_var(Env10, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env10, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_generate1(Con_Get,
		      Xx_global_switches_xx_Get,
		      Xx_reality_xx_Get,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_gn, classof, claz_function),
   set_opv(u_gn, compile_as, kw_function),
   set_opv(u_gn, function, f_u_gn),
   DefunResult=u_gn.
/*
:- side_effect(assert_lsp(u_gn,
			  wl:lambda_def(defun, u_gn, f_u_gn, [u_con], [[u_generate1, u_con, u_xx_global_switches_xx, u_xx_reality_xx, u_xx_me_belief_path_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_gn,
			  wl:arglist_info(u_gn, f_u_gn, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gn, wl:init_args(exact_only, f_u_gn))).
*/
/*
(defun gns (con sw) (generate1 con sw *reality* *me-belief-path*))

; This function should be called only from assert.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:559 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,gns,[con,sw],[generate1,con,sw,'*reality*','*me-belief-path*']])
wl:lambda_def(defun, u_gns, f_u_gns, [u_con, u_sw], [[u_generate1, u_con, u_sw, u_xx_reality_xx, u_xx_me_belief_path_xx]]).
wl:arglist_info(u_gns, f_u_gns, [u_con, u_sw], arginfo{all:[u_con, u_sw], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_sw], opt:0, req:[u_con, u_sw], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gns).

/*

### Compiled:  `U::GNS` 
*/
f_u_gns(Con, Sw, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_con, Con), bv(u_sw, Sw)|Env],
	get_var(Env10, u_con, Con_Get),
	get_var(Env10, u_sw, Sw_Get),
	get_var(Env10, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env10, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_generate1(Con_Get,
		      Sw_Get,
		      Xx_reality_xx_Get,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_gns, classof, claz_function),
   set_opv(u_gns, compile_as, kw_function),
   set_opv(u_gns, function, f_u_gns),
   DefunResult=u_gns.
/*
:- side_effect(assert_lsp(u_gns,
			  wl:lambda_def(defun, u_gns, f_u_gns, [u_con, u_sw], [[u_generate1, u_con, u_sw, u_xx_reality_xx, u_xx_me_belief_path_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_gns,
			  wl:arglist_info(u_gns, f_u_gns, [u_con, u_sw], arginfo{all:[u_con, u_sw], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_sw], opt:0, req:[u_con, u_sw], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gns, wl:init_args(exact_only, f_u_gns))).
*/
/*
 This function should be called only from assert.
*/
/*
(defun generate (con context switches)
 (if (and (null? (switches-lookup 'no-gen switches))
          (null? (no-top-gen? con context)))
     (progn
;      (setq *references* nil)
      (gs-reset-sentence *gen-stream*) ; just for good measure.
      (gs-string-write *gen-stream*
       "==================================================")
      (gs-newline *gen-stream*)
      (generate1 con switches context *me-belief-path*)
      (gs-string-write *gen-stream*
       "==================================================")
      (gs-newline *gen-stream*))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:678 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,generate,[con,context,switches],[if,[and,['null?',['switches-lookup',[quote,'no-gen'],switches]],['null?',['no-top-gen?',con,context]]],[progn,['gs-reset-sentence','*gen-stream*'],['gs-string-write','*gen-stream*','$STRING'("==================================================")],['gs-newline','*gen-stream*'],[generate1,con,switches,context,'*me-belief-path*'],['gs-string-write','*gen-stream*','$STRING'("==================================================")],['gs-newline','*gen-stream*']]]])
wl:lambda_def(defun, u_generate, f_u_generate, [u_con, u_context, u_switches], [[if, [and, [u_null_c63, [u_switches_lookup, [quote, u_no_gen], u_switches]], [u_null_c63, [u_no_top_gen_c63, u_con, u_context]]], [progn, [u_gs_reset_sentence, u_xx_gen_stream_xx], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "==================================================")], [u_gs_newline, u_xx_gen_stream_xx], [u_generate1, u_con, u_switches, u_context, u_xx_me_belief_path_xx], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "==================================================")], [u_gs_newline, u_xx_gen_stream_xx]]]]).
wl:arglist_info(u_generate, f_u_generate, [u_con, u_context, u_switches], arginfo{all:[u_con, u_context, u_switches], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_context, u_switches], opt:0, req:[u_con, u_context, u_switches], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_generate).

/*

### Compiled:  `U::GENERATE` 
*/
f_u_generate(Con, Context, Switches, FnResult) :-
	nop(global_env(Env)),
	Env21=[bv(u_con, Con), bv(u_context, Context), bv(u_switches, Switches)|Env],
	f_u_null_c63([u_switches_lookup, [quote, u_no_gen], u_switches], IFTEST6),
	(   IFTEST6\==[]
	->  f_u_null_c63([u_no_top_gen_c63, u_con, u_context], TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env21, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	    f_u_gs_reset_sentence(Xx_gen_stream_xx_Get, Reset_sentence_Ret),
	    get_var(Env21, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get10),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get10,
				'$ARRAY'([*],
					 claz_base_character,
					 "=================================================="),
				String_write_Ret),
	    get_var(Env21, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get11),
	    f_u_gs_newline(Xx_gen_stream_xx_Get11, Gs_newline_Ret),
	    get_var(Env21, u_con, Con_Get),
	    get_var(Env21, u_context, Context_Get),
	    get_var(Env21, u_switches, Switches_Get),
	    get_var(Env21, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_generate1(Con_Get,
			  Switches_Get,
			  Context_Get,
			  Xx_me_belief_path_xx_Get,
			  Generate1_Ret),
	    get_var(Env21, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get16),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get16,
				'$ARRAY'([*],
					 claz_base_character,
					 "=================================================="),
				String_write_Ret29),
	    get_var(Env21, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get17),
	    f_u_gs_newline(Xx_gen_stream_xx_Get17, TrueResult18),
	    FnResult=TrueResult18
	;   FnResult=[]
	).
:- set_opv(f_u_generate, classof, claz_function),
   set_opv(u_generate, compile_as, kw_function),
   set_opv(u_generate, function, f_u_generate),
   DefunResult=u_generate.
/*
:- side_effect(assert_lsp(u_generate,
			  wl:lambda_def(defun, u_generate, f_u_generate, [u_con, u_context, u_switches], [[if, [and, [u_null_c63, [u_switches_lookup, [quote, u_no_gen], u_switches]], [u_null_c63, [u_no_top_gen_c63, u_con, u_context]]], [progn, [u_gs_reset_sentence, u_xx_gen_stream_xx], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "==================================================")], [u_gs_newline, u_xx_gen_stream_xx], [u_generate1, u_con, u_switches, u_context, u_xx_me_belief_path_xx], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, "==================================================")], [u_gs_newline, u_xx_gen_stream_xx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_generate,
			  wl:arglist_info(u_generate, f_u_generate, [u_con, u_context, u_switches], arginfo{all:[u_con, u_context, u_switches], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_context, u_switches], opt:0, req:[u_con, u_context, u_switches], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_generate, wl:init_args(exact_only, f_u_generate))).
*/
/*
      (setq *references* nil)
*/
/*
 just for good measure.
*/
/*
(setq *no-top-gen-lst* (list ^rprox ^causal-link ^other ^ordering
                             ^altern ^at))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1240 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*no-top-gen-lst*',[list,'^rprox','^causal-link','^other','^ordering','^altern','^at']])
:- get_var(AEnv, u_c94_causal_link, C94_causal_link_Get),
   get_var(AEnv, u_c94_ordering, C94_ordering_Get),
   ( get_var(AEnv, u_c94_altern, C94_altern_Get),
     get_var(AEnv, u_c94_rprox, C94_rprox_Get)
   ),
   ( get_var(AEnv, u_c94_at, C94_at_Get),
     get_var(AEnv, u_c94_other, C94_other_Get)
   ),
   _Ignored=[C94_rprox_Get, C94_causal_link_Get, C94_other_Get, C94_ordering_Get, C94_altern_Get, C94_at_Get],
   set_var(AEnv, u_xx_no_top_gen_lst_xx, _Ignored).
/*
(defun no-top-gen? (con context)
  (and (ob? con)
       (not (ob$get con 'input-state?))
  (or (instance-of-any? con *no-top-gen-lst*)
      (action-goal-success? con)
      (believe-action? con)
      (believe-introduction? con)
      (believe-rprox? con)
      (believe-link? con)
      (know-location? con)
      (very-small-emotion? con)
      (rtrue-subgoal? con))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1350 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'no-top-gen?',[con,context],[and,['ob?',con],[not,['ob$get',con,[quote,'input-state?']]],[or,['instance-of-any?',con,'*no-top-gen-lst*'],['action-goal-success?',con],['believe-action?',con],['believe-introduction?',con],['believe-rprox?',con],['believe-link?',con],['know-location?',con],['very-small-emotion?',con],['rtrue-subgoal?',con]]]])
wl:lambda_def(defun, u_no_top_gen_c63, f_u_no_top_gen_c63, [u_con, u_context], [[and, [u_ob_c63, u_con], [not, [u_ob_c36_get, u_con, [quote, u_input_state_c63]]], [or, [u_instance_of_any_c63, u_con, u_xx_no_top_gen_lst_xx], [u_action_goal_success_c63, u_con], [u_believe_action_c63, u_con], [u_believe_introduction_c63, u_con], [u_believe_rprox_c63, u_con], [u_believe_link_c63, u_con], [u_know_location_c63, u_con], [u_very_small_emotion_c63, u_con], [u_rtrue_subgoal_c63, u_con]]]]).
wl:arglist_info(u_no_top_gen_c63, f_u_no_top_gen_c63, [u_con, u_context], arginfo{all:[u_con, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_context], opt:0, req:[u_con, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_no_top_gen_c63).

/*

### Compiled:  `U::NO-TOP-GEN?` 
*/
f_u_no_top_gen_c63(Con, Context, TrueResult29) :-
	nop(global_env(Env)),
	Env32=[bv(u_con, Con), bv(u_context, Context)|Env],
	f_u_ob_c63(u_con, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env32, u_con, Con_Get),
	    f_u_ob_c36_get(Con_Get, u_input_state_c63, PredArgResult),
	    (   PredArgResult==[]
	    ->  (   get_var(Env32, u_con, Con_Get10),
		    get_var(Env32,
			    u_xx_no_top_gen_lst_xx,
			    Xx_no_top_gen_lst_xx_Get),
		    f_u_instance_of_any_c63(Con_Get10,
					    Xx_no_top_gen_lst_xx_Get,
					    FORM1_Res27),
		    FORM1_Res27\==[],
		    TrueResult29=FORM1_Res27
		->  true
		;   get_var(Env32, u_con, Con_Get12),
		    f_u_action_goal_success_c63(Con_Get12, FORM1_Res26),
		    FORM1_Res26\==[],
		    TrueResult29=FORM1_Res26
		->  true
		;   get_var(Env32, u_con, Con_Get13),
		    f_u_believe_action_c63(Con_Get13, FORM1_Res25),
		    FORM1_Res25\==[],
		    TrueResult29=FORM1_Res25
		->  true
		;   get_var(Env32, u_con, Con_Get14),
		    f_u_believe_introduction_c63(Con_Get14, FORM1_Res24),
		    FORM1_Res24\==[],
		    TrueResult29=FORM1_Res24
		->  true
		;   get_var(Env32, u_con, Con_Get15),
		    f_u_believe_rprox_c63(Con_Get15, FORM1_Res23),
		    FORM1_Res23\==[],
		    TrueResult29=FORM1_Res23
		->  true
		;   get_var(Env32, u_con, Con_Get16),
		    f_u_believe_link_c63(Con_Get16, FORM1_Res22),
		    FORM1_Res22\==[],
		    TrueResult29=FORM1_Res22
		->  true
		;   get_var(Env32, u_con, Con_Get17),
		    f_u_know_location_c63(Con_Get17, FORM1_Res21),
		    FORM1_Res21\==[],
		    TrueResult29=FORM1_Res21
		->  true
		;   get_var(Env32, u_con, Con_Get18),
		    f_u_very_small_emotion_c63(Con_Get18, FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult29=FORM1_Res
		->  true
		;   get_var(Env32, u_con, Con_Get19),
		    f_u_rtrue_subgoal_c63(Con_Get19, Subgoal_c63_Ret),
		    TrueResult29=Subgoal_c63_Ret
		)
	    ;   TrueResult29=[]
	    )
	;   TrueResult29=[]
	).
:- set_opv(f_u_no_top_gen_c63, classof, claz_function),
   set_opv(u_no_top_gen_c63, compile_as, kw_function),
   set_opv(u_no_top_gen_c63, function, f_u_no_top_gen_c63),
   DefunResult=u_no_top_gen_c63.
/*
:- side_effect(assert_lsp(u_no_top_gen_c63,
			  wl:lambda_def(defun, u_no_top_gen_c63, f_u_no_top_gen_c63, [u_con, u_context], [[and, [u_ob_c63, u_con], [not, [u_ob_c36_get, u_con, [quote, u_input_state_c63]]], [or, [u_instance_of_any_c63, u_con, u_xx_no_top_gen_lst_xx], [u_action_goal_success_c63, u_con], [u_believe_action_c63, u_con], [u_believe_introduction_c63, u_con], [u_believe_rprox_c63, u_con], [u_believe_link_c63, u_con], [u_know_location_c63, u_con], [u_very_small_emotion_c63, u_con], [u_rtrue_subgoal_c63, u_con]]]]))).
*/
/*
:- side_effect(assert_lsp(u_no_top_gen_c63,
			  wl:arglist_info(u_no_top_gen_c63, f_u_no_top_gen_c63, [u_con, u_context], arginfo{all:[u_con, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_context], opt:0, req:[u_con, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_no_top_gen_c63,
			  wl:init_args(exact_only, f_u_no_top_gen_c63))).
*/
/*
(defun very-small-emotion? (ob)
  (and (ty$instance? ob 'emotion)
       (fl< (strength ob) 0.1)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1724 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'very-small-emotion?',[ob],[and,['ty$instance?',ob,[quote,emotion]],['fl<',[strength,ob],0.1]]])
wl:lambda_def(defun, u_very_small_emotion_c63, f_u_very_small_emotion_c63, [u_ob], [[and, [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]], [u_fl_c60, [u_strength, u_ob], 0.1]]]).
wl:arglist_info(u_very_small_emotion_c63, f_u_very_small_emotion_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_very_small_emotion_c63).

/*

### Compiled:  `U::VERY-SMALL-EMOTION?` 
*/
f_u_very_small_emotion_c63(Ob, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_ob, Ob)|Env],
	get_var(Env10, u_ob, Ob_Get),
	f_u_ty_c36_instance_c63(Ob_Get, u_emotion, IFTEST),
	(   IFTEST\==[]
	->  f_u_fl_c60([u_strength, u_ob], 0.1, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_very_small_emotion_c63, classof, claz_function),
   set_opv(u_very_small_emotion_c63, compile_as, kw_function),
   set_opv(u_very_small_emotion_c63, function, f_u_very_small_emotion_c63),
   DefunResult=u_very_small_emotion_c63.
/*
:- side_effect(assert_lsp(u_very_small_emotion_c63,
			  wl:lambda_def(defun, u_very_small_emotion_c63, f_u_very_small_emotion_c63, [u_ob], [[and, [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]], [u_fl_c60, [u_strength, u_ob], 0.1]]]))).
*/
/*
:- side_effect(assert_lsp(u_very_small_emotion_c63,
			  wl:arglist_info(u_very_small_emotion_c63, f_u_very_small_emotion_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_very_small_emotion_c63,
			  wl:init_args(exact_only, f_u_very_small_emotion_c63))).
*/
/*
(defun instance-of-any? (ob lst)
  (any? (lambda (x) (ty$instance-of? ob x)) lst))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1824 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'instance-of-any?',[ob,lst],['any?',[lambda,[x],['ty$instance-of?',ob,x]],lst]])
wl:lambda_def(defun, u_instance_of_any_c63, f_u_instance_of_any_c63, [u_ob, u_lst], [[u_any_c63, [lambda, [u_x], [u_ty_c36_instance_of_c63, u_ob, u_x]], u_lst]]).
wl:arglist_info(u_instance_of_any_c63, f_u_instance_of_any_c63, [u_ob, u_lst], arginfo{all:[u_ob, u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_lst], opt:0, req:[u_ob, u_lst], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_instance_of_any_c63).

/*

### Compiled:  `U::INSTANCE-OF-ANY?` 
*/
f_u_instance_of_any_c63(Ob, Lst, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_ob, Ob), bv(u_lst, Lst)|Env],
	f_u_any_c63([lambda, [u_x], [u_ty_c36_instance_of_c63, u_ob, u_x]],
		    u_lst,
		    Lst9),
	Lst9=FnResult.
:- set_opv(f_u_instance_of_any_c63, classof, claz_function),
   set_opv(u_instance_of_any_c63, compile_as, kw_function),
   set_opv(u_instance_of_any_c63, function, f_u_instance_of_any_c63),
   DefunResult=u_instance_of_any_c63.
/*
:- side_effect(assert_lsp(u_instance_of_any_c63,
			  wl:lambda_def(defun, u_instance_of_any_c63, f_u_instance_of_any_c63, [u_ob, u_lst], [[u_any_c63, [lambda, [u_x], [u_ty_c36_instance_of_c63, u_ob, u_x]], u_lst]]))).
*/
/*
:- side_effect(assert_lsp(u_instance_of_any_c63,
			  wl:arglist_info(u_instance_of_any_c63, f_u_instance_of_any_c63, [u_ob, u_lst], arginfo{all:[u_ob, u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_lst], opt:0, req:[u_ob, u_lst], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_instance_of_any_c63,
			  wl:init_args(exact_only, f_u_instance_of_any_c63))).
*/
/*
(defun believe-action? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'ACTION)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1908 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'believe-action?',[con],[and,['ty$instance?',con,[quote,'BELIEVE']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'ACTION']]]])
wl:lambda_def(defun, u_believe_action_c63, f_u_believe_action_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_action]]]]).
wl:arglist_info(u_believe_action_c63, f_u_believe_action_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_believe_action_c63).

/*

### Compiled:  `U::BELIEVE-ACTION?` 
*/
f_u_believe_action_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_action, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_believe_action_c63, classof, claz_function),
   set_opv(u_believe_action_c63, compile_as, kw_function),
   set_opv(u_believe_action_c63, function, f_u_believe_action_c63),
   DefunResult=u_believe_action_c63.
/*
:- side_effect(assert_lsp(u_believe_action_c63,
			  wl:lambda_def(defun, u_believe_action_c63, f_u_believe_action_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_action]]]]))).
*/
/*
:- side_effect(assert_lsp(u_believe_action_c63,
			  wl:arglist_info(u_believe_action_c63, f_u_believe_action_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_believe_action_c63,
			  wl:init_args(exact_only, f_u_believe_action_c63))).
*/
/*
(defun action-goal-success? (con)
  (and (ty$instance? con 'SUCCEEDED-GOAL)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'ACTION)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2054 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'action-goal-success?',[con],[and,['ty$instance?',con,[quote,'SUCCEEDED-GOAL']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'ACTION']]]])
wl:lambda_def(defun, u_action_goal_success_c63, f_u_action_goal_success_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_succeeded_goal]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_action]]]]).
wl:arglist_info(u_action_goal_success_c63, f_u_action_goal_success_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_action_goal_success_c63).

/*

### Compiled:  `U::ACTION-GOAL-SUCCESS?` 
*/
f_u_action_goal_success_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_succeeded_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_action, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_action_goal_success_c63, classof, claz_function),
   set_opv(u_action_goal_success_c63, compile_as, kw_function),
   set_opv(u_action_goal_success_c63, function, f_u_action_goal_success_c63),
   DefunResult=u_action_goal_success_c63.
/*
:- side_effect(assert_lsp(u_action_goal_success_c63,
			  wl:lambda_def(defun, u_action_goal_success_c63, f_u_action_goal_success_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_succeeded_goal]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_action]]]]))).
*/
/*
:- side_effect(assert_lsp(u_action_goal_success_c63,
			  wl:arglist_info(u_action_goal_success_c63, f_u_action_goal_success_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_action_goal_success_c63,
			  wl:init_args(exact_only, f_u_action_goal_success_c63))).
*/
/*
(defun believe-introduction? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'INTRODUCTION)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2212 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'believe-introduction?',[con],[and,['ty$instance?',con,[quote,'BELIEVE']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'INTRODUCTION']]]])
wl:lambda_def(defun, u_believe_introduction_c63, f_u_believe_introduction_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_introduction]]]]).
wl:arglist_info(u_believe_introduction_c63, f_u_believe_introduction_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_believe_introduction_c63).

/*

### Compiled:  `U::BELIEVE-INTRODUCTION?` 
*/
f_u_believe_introduction_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_introduction, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_believe_introduction_c63, classof, claz_function),
   set_opv(u_believe_introduction_c63, compile_as, kw_function),
   set_opv(u_believe_introduction_c63, function, f_u_believe_introduction_c63),
   DefunResult=u_believe_introduction_c63.
/*
:- side_effect(assert_lsp(u_believe_introduction_c63,
			  wl:lambda_def(defun, u_believe_introduction_c63, f_u_believe_introduction_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_introduction]]]]))).
*/
/*
:- side_effect(assert_lsp(u_believe_introduction_c63,
			  wl:arglist_info(u_believe_introduction_c63, f_u_believe_introduction_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_believe_introduction_c63,
			  wl:init_args(exact_only, f_u_believe_introduction_c63))).
*/
/*
(defun believe-link? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'CAUSAL-LINK)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2370 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'believe-link?',[con],[and,['ty$instance?',con,[quote,'BELIEVE']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'CAUSAL-LINK']]]])
wl:lambda_def(defun, u_believe_link_c63, f_u_believe_link_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_causal_link]]]]).
wl:arglist_info(u_believe_link_c63, f_u_believe_link_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_believe_link_c63).

/*

### Compiled:  `U::BELIEVE-LINK?` 
*/
f_u_believe_link_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_causal_link, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_believe_link_c63, classof, claz_function),
   set_opv(u_believe_link_c63, compile_as, kw_function),
   set_opv(u_believe_link_c63, function, f_u_believe_link_c63),
   DefunResult=u_believe_link_c63.
/*
:- side_effect(assert_lsp(u_believe_link_c63,
			  wl:lambda_def(defun, u_believe_link_c63, f_u_believe_link_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_causal_link]]]]))).
*/
/*
:- side_effect(assert_lsp(u_believe_link_c63,
			  wl:arglist_info(u_believe_link_c63, f_u_believe_link_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_believe_link_c63,
			  wl:init_args(exact_only, f_u_believe_link_c63))).
*/
/*
(defun believe-rprox? (con)
  (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'RPROX)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2519 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'believe-rprox?',[con],[and,['ty$instance?',con,[quote,'BELIEVE']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'RPROX']]]])
wl:lambda_def(defun, u_believe_rprox_c63, f_u_believe_rprox_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_rprox]]]]).
wl:arglist_info(u_believe_rprox_c63, f_u_believe_rprox_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_believe_rprox_c63).

/*

### Compiled:  `U::BELIEVE-RPROX?` 
*/
f_u_believe_rprox_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_rprox, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_believe_rprox_c63, classof, claz_function),
   set_opv(u_believe_rprox_c63, compile_as, kw_function),
   set_opv(u_believe_rprox_c63, function, f_u_believe_rprox_c63),
   DefunResult=u_believe_rprox_c63.
/*
:- side_effect(assert_lsp(u_believe_rprox_c63,
			  wl:lambda_def(defun, u_believe_rprox_c63, f_u_believe_rprox_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_rprox]]]]))).
*/
/*
:- side_effect(assert_lsp(u_believe_rprox_c63,
			  wl:arglist_info(u_believe_rprox_c63, f_u_believe_rprox_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_believe_rprox_c63,
			  wl:init_args(exact_only, f_u_believe_rprox_c63))).
*/
/*
(defun know-location? (con)
  (and (ty$instance? con 'KNOW)
       (ob? (ob$get con 'obj))
       (ty$instance? (ob$get con 'obj) 'LOCATION)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2663 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'know-location?',[con],[and,['ty$instance?',con,[quote,'KNOW']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'LOCATION']]]])
wl:lambda_def(defun, u_know_location_c63, f_u_know_location_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_know]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_location]]]]).
wl:arglist_info(u_know_location_c63, f_u_know_location_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_know_location_c63).

/*

### Compiled:  `U::KNOW-LOCATION?` 
*/
f_u_know_location_c63(Con, TrueResult11) :-
	nop(global_env(Env)),
	Env14=[bv(u_con, Con)|Env],
	get_var(Env14, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_know, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(Env14, u_con, Con_Get9),
		f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_location, TrueResult),
		TrueResult11=TrueResult
	    ;   TrueResult11=[]
	    )
	;   TrueResult11=[]
	).
:- set_opv(f_u_know_location_c63, classof, claz_function),
   set_opv(u_know_location_c63, compile_as, kw_function),
   set_opv(u_know_location_c63, function, f_u_know_location_c63),
   DefunResult=u_know_location_c63.
/*
:- side_effect(assert_lsp(u_know_location_c63,
			  wl:lambda_def(defun, u_know_location_c63, f_u_know_location_c63, [u_con], [[and, [u_ty_c36_instance_c63, u_con, [quote, u_know]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_location]]]]))).
*/
/*
:- side_effect(assert_lsp(u_know_location_c63,
			  wl:arglist_info(u_know_location_c63, f_u_know_location_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_know_location_c63,
			  wl:init_args(exact_only, f_u_know_location_c63))).
*/
/*
(defun rtrue-subgoal? (con)
  (or (and (ty$instance? con 'ACTIVE-GOAL)
       (ob? (ob$get con 'obj))
           (ty$instance? (ob$get con 'obj) 'RTRUE))
      (and (ty$instance? con 'BELIEVE)
       (ob? (ob$get con 'obj))
           (ty$instance? (ob$get con 'obj) 'ACTIVE-GOAL)
       (ob? (ob$pget con '(obj obj)))
           (ty$instance? (ob$pget con '(obj obj)) 'RTRUE))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2807 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'rtrue-subgoal?',[con],[or,[and,['ty$instance?',con,[quote,'ACTIVE-GOAL']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'RTRUE']]],[and,['ty$instance?',con,[quote,'BELIEVE']],['ob?',['ob$get',con,[quote,obj]]],['ty$instance?',['ob$get',con,[quote,obj]],[quote,'ACTIVE-GOAL']],['ob?',['ob$pget',con,[quote,[obj,obj]]]],['ty$instance?',['ob$pget',con,[quote,[obj,obj]]],[quote,'RTRUE']]]]])
wl:lambda_def(defun, u_rtrue_subgoal_c63, f_u_rtrue_subgoal_c63, [u_con], [[or, [and, [u_ty_c36_instance_c63, u_con, [quote, u_active_goal]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_rtrue]]], [and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_active_goal]], [u_ob_c63, [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]]], [u_ty_c36_instance_c63, [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]], [quote, u_rtrue]]]]]).
wl:arglist_info(u_rtrue_subgoal_c63, f_u_rtrue_subgoal_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_rtrue_subgoal_c63).

/*

### Compiled:  `U::RTRUE-SUBGOAL?` 
*/
f_u_rtrue_subgoal_c63(Con, TrueResult24) :-
	nop(global_env(Env)),
	Env30=[bv(u_con, Con)|Env],
	(   get_var(Env30, u_con, Con_Get),
	    f_u_ty_c36_instance_c63(Con_Get, u_active_goal, IFTEST),
	    (   IFTEST\==[]
	    ->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST7),
		(   IFTEST7\==[]
		->  get_var(Env30, u_con, Con_Get9),
		    f_u_ob_c36_get(Con_Get9, u_obj, Obj),
		    f_u_ty_c36_instance_c63(Obj, u_rtrue, TrueResult),
		    TrueResult11=TrueResult
		;   TrueResult11=[]
		)
	    ;   TrueResult11=[]
	    ),
	    TrueResult11\==[],
	    TrueResult24=TrueResult11
	->  true
	;   get_var(Env30, u_con, Con_Get14),
	    f_u_ty_c36_instance_c63(Con_Get14, u_believe, IFTEST12),
	    (   IFTEST12\==[]
	    ->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
		(   IFTEST15\==[]
		->  get_var(Env30, u_con, Con_Get19),
		    f_u_ob_c36_get(Con_Get19, u_obj, Obj33),
		    f_u_ty_c36_instance_c63(Obj33, u_active_goal, IFTEST17),
		    (   IFTEST17\==[]
		    ->  f_u_ob_c63([u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]],
				   IFTEST20),
			(   IFTEST20\==[]
			->  get_var(Env30, u_con, Con_Get22),
			    f_u_ob_c36_pget(Con_Get22,
					    [u_obj, u_obj],
					    Instance_c63_Param),
			    f_u_ty_c36_instance_c63(Instance_c63_Param,
						    u_rtrue,
						    TrueResult23),
			    TrueResult24=TrueResult23
			;   TrueResult24=[]
			)
		    ;   TrueResult24=[]
		    )
		;   TrueResult24=[]
		)
	    ;   TrueResult24=[]
	    )
	).
:- set_opv(f_u_rtrue_subgoal_c63, classof, claz_function),
   set_opv(u_rtrue_subgoal_c63, compile_as, kw_function),
   set_opv(u_rtrue_subgoal_c63, function, f_u_rtrue_subgoal_c63),
   DefunResult=u_rtrue_subgoal_c63.
/*
:- side_effect(assert_lsp(u_rtrue_subgoal_c63,
			  wl:lambda_def(defun, u_rtrue_subgoal_c63, f_u_rtrue_subgoal_c63, [u_con], [[or, [and, [u_ty_c36_instance_c63, u_con, [quote, u_active_goal]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_rtrue]]], [and, [u_ty_c36_instance_c63, u_con, [quote, u_believe]], [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]], [u_ty_c36_instance_c63, [u_ob_c36_get, u_con, [quote, u_obj]], [quote, u_active_goal]], [u_ob_c63, [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]]], [u_ty_c36_instance_c63, [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]], [quote, u_rtrue]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_rtrue_subgoal_c63,
			  wl:arglist_info(u_rtrue_subgoal_c63, f_u_rtrue_subgoal_c63, [u_con], arginfo{all:[u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con], opt:0, req:[u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_rtrue_subgoal_c63,
			  wl:init_args(exact_only, f_u_rtrue_subgoal_c63))).
*/
/*
(defun discourse-reset ()
 (setq *references* nil))

;
; Imagined future assumption === past-subjunctive
; "What if I went to the store?"
; Imagined future === conditional
; "I would go to the store"
; Imagined past assumption === past-perfect
; "What if I had gone to the store?"
; Imagined past === conditional-present-perfect
; "I would have gone to the store"
;

; The below doesn't work (?) because obs can change (or are they copied
; for context's sake?)
;
;(setq *gen-history* nil)
;
;(defun gen-history ()
;  (setq *references* nil)
;  (yloop (yfor item in (reverse *gen-history*))
;        (ydo (generate1 (car item)
;                       (cadr item)
;                       (caddr item)
;                       (cadddr item)))))

; Will add paragraph to English-only trace.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'discourse-reset',[],[setq,'*references*',[]]])
wl:lambda_def(defun, u_discourse_reset, f_u_discourse_reset, [], [[setq, u_xx_references_xx, []]]).
wl:arglist_info(u_discourse_reset, f_u_discourse_reset, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_discourse_reset).

/*

### Compiled:  `U::DISCOURSE-RESET` 
*/
f_u_discourse_reset(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	set_var(AEnv, setq, u_xx_references_xx, []),
	[]=FnResult.
:- set_opv(f_u_discourse_reset, classof, claz_function),
   set_opv(u_discourse_reset, compile_as, kw_function),
   set_opv(u_discourse_reset, function, f_u_discourse_reset),
   DefunResult=u_discourse_reset.
/*
:- side_effect(assert_lsp(u_discourse_reset,
			  wl:lambda_def(defun, u_discourse_reset, f_u_discourse_reset, [], [[setq, u_xx_references_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_discourse_reset,
			  wl:arglist_info(u_discourse_reset, f_u_discourse_reset, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_discourse_reset,
			  wl:init_args(exact_only, f_u_discourse_reset))).
*/
/*
*/
/*
 Imagined future assumption === past-subjunctive
*/
/*
 "What if I went to the store?"
*/
/*
 Imagined future === conditional
*/
/*
 "I would go to the store"
*/
/*
 Imagined past assumption === past-perfect
*/
/*
 "What if I had gone to the store?"
*/
/*
 Imagined past === conditional-present-perfect
*/
/*
 "I would have gone to the store"
*/
/*
*/
/*
 The below doesn't work (?) because obs can change (or are they copied
*/
/*
 for context's sake?)
*/
/*
*/
/*
(setq *gen-history* nil)
*/
/*
*/
/*
(defun gen-history ()
*/
/*
  (setq *references* nil)
*/
/*
  (yloop (yfor item in (reverse *gen-history*))
*/
/*
        (ydo (generate1 (car item)
*/
/*
                       (cadr item)
*/
/*
                       (caddr item)
*/
/*
                       (cadddr item)))))
*/
/*
 Will add paragraph to English-only trace.
*/
/*
(defun gen-new-paragraph ()
  (setq *references* nil)
  nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3975 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'gen-new-paragraph',[],[setq,'*references*',[]],[]])
wl:lambda_def(defun, u_gen_new_paragraph, f_u_gen_new_paragraph, [], [[setq, u_xx_references_xx, []], []]).
wl:arglist_info(u_gen_new_paragraph, f_u_gen_new_paragraph, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gen_new_paragraph).

/*

### Compiled:  `U::GEN-NEW-PARAGRAPH` 
*/
f_u_gen_new_paragraph(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	set_var(AEnv, setq, u_xx_references_xx, []),
	[]=FnResult.
:- set_opv(f_u_gen_new_paragraph, classof, claz_function),
   set_opv(u_gen_new_paragraph, compile_as, kw_function),
   set_opv(u_gen_new_paragraph, function, f_u_gen_new_paragraph),
   DefunResult=u_gen_new_paragraph.
/*
:- side_effect(assert_lsp(u_gen_new_paragraph,
			  wl:lambda_def(defun, u_gen_new_paragraph, f_u_gen_new_paragraph, [], [[setq, u_xx_references_xx, []], []]))).
*/
/*
:- side_effect(assert_lsp(u_gen_new_paragraph,
			  wl:arglist_info(u_gen_new_paragraph, f_u_gen_new_paragraph, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gen_new_paragraph,
			  wl:init_args(exact_only, f_u_gen_new_paragraph))).
*/
/*
(setq *possibly-realism* 0.3)

;   Perf   Dd
;   ------------------+
;   Maybe    Say      | Subgoal relaxation
;   Possibly Possibly | Plausible planning

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*possibly-realism*',0.3])
:- set_var(AEnv, setq, u_xx_possibly_realism_xx, 0.3).
/*
   Perf   Dd
*/
/*
   ------------------+
*/
/*
   Maybe    Say      | Subgoal relaxation
*/
/*
   Possibly Possibly | Plausible planning
*/
/*
(defun generate1 (con switches context bp)
  (let ((gened? nil))
;   (setq *gen-history* (cons (list con switches context bp) *gen-history*))
   (if *typeset?*
       (format *gate-dbg* "\\vspace{2 mm}"(defun generate1 (con switches context bp)\n  (let ((gened? nil))\n;   (setq *gen-history* (cons (list con switches context bp) *gen-history*))\n   (if *typeset?*\n       (format *gate-dbg* \"\\\\vspace{2 mm}~%\"))\n   (ndbg-large-bold-font *gate-dbg* rule)\n   (gs-reset-sentence *gen-stream*)\n   (gs-reset-line *gen-stream*)\n   (if (and (< (strength con) *possibly-realism*)\n            (not (ty$instance? con 'surprise))\n            (not (ty$instance? con 'overall-emotion))\n            (not (switches-lookup 'tongue-in-cheek switches)))\n       (gs-string-write *gen-stream* \" possibly\"))\n   (cond\n    ((switches-lookup 'what-if switches)\n     (gs-string-write *gen-stream* \" what\")\n     (gs-string-write *gen-stream* \" if\")\n     (if (setq gened? (gen con *gen-stream* switches context bp))\n         (progn\n          (justify-if-desired con *gen-stream* switches context bp)\n          (gs-end-question *gen-stream*))))\n    ((switches-lookup 'tongue-in-cheek switches)\n     (gs-string-write *gen-stream* \" anyway,\")\n     (if (setq gened? (gen con *gen-stream* switches context bp))\n         (progn\n          (justify-if-desired con *gen-stream* switches context bp)\n          (gs-end-sentence *gen-stream*))))\n    ((switches-lookup 'relaxation switches)\n     (if (performance-mode?)\n         (gs-string-write *gen-stream* \" maybe\")\n         (gs-string-write *gen-stream* \" say\"))\n     (if (setq gened? (gen con *gen-stream* switches context bp))\n         (progn\n          (justify-if-desired con *gen-stream* switches context bp)\n          (gs-end-sentence *gen-stream*))))\n    ((switches-lookup 'backtrack switches)\n     (gs-string-write *gen-stream* \" no\")\n     (setq gened? t)\n;     (gs-end-sentence *gen-stream*)\n;     (gs-string-write *gen-stream* \" instead of\")\n;     (setq gened? (gen con *gen-stream* switches context bp))\n;     (gs-string-write *gen-stream* \", how about this\")\n;     ; A justification will never be used here.\n     (gs-end-sentence *gen-stream*))\n    (else\n     (if (setq gened? (gen con *gen-stream* switches context bp))\n         (progn\n          (justify-if-desired con *gen-stream* switches context bp)\n          (if (ty$instance? con 'surprise)\n              (gs-end-exclam *gen-stream*)\n              (gs-end-sentence *gen-stream*))))))\n    (ndbg-end-font *gate-dbg* rule)\n    (if gened?\n        (progn\n         (if *typeset?*\n             (format *gate-dbg* \"\\\\vspace{2 mm}~%\"))\n         (if (null? (ob$get context 'first-gened-concept))\n             (ob$set context 'first-gened-concept con))))\n    (if (and nil ; (ty$instance? con 'emotion) No more overall gen\n             (not (ty$instance? con 'surprise))\n             (not (ty$instance? con 'overall-emotion)))\n        (gen-overall-emot-state))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,generate1,[con,switches,context,bp],[let,[['gened?',[]]],[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\vspace{2 mm}~%")]],['ndbg-large-bold-font','*gate-dbg*',rule],['gs-reset-sentence','*gen-stream*'],['gs-reset-line','*gen-stream*'],[if,[and,[<,[strength,con],'*possibly-realism*'],[not,['ty$instance?',con,[quote,surprise]]],[not,['ty$instance?',con,[quote,'overall-emotion']]],[not,['switches-lookup',[quote,'tongue-in-cheek'],switches]]],['gs-string-write','*gen-stream*','$STRING'(" possibly")]],[cond,[['switches-lookup',[quote,'what-if'],switches],['gs-string-write','*gen-stream*','$STRING'(" what")],['gs-string-write','*gen-stream*','$STRING'(" if")],[if,[setq,'gened?',[gen,con,'*gen-stream*',switches,context,bp]],[progn,['justify-if-desired',con,'*gen-stream*',switches,context,bp],['gs-end-question','*gen-stream*']]]],[['switches-lookup',[quote,'tongue-in-cheek'],switches],['gs-string-write','*gen-stream*','$STRING'(" anyway,")],[if,[setq,'gened?',[gen,con,'*gen-stream*',switches,context,bp]],[progn,['justify-if-desired',con,'*gen-stream*',switches,context,bp],['gs-end-sentence','*gen-stream*']]]],[['switches-lookup',[quote,relaxation],switches],[if,['performance-mode?'],['gs-string-write','*gen-stream*','$STRING'(" maybe")],['gs-string-write','*gen-stream*','$STRING'(" say")]],[if,[setq,'gened?',[gen,con,'*gen-stream*',switches,context,bp]],[progn,['justify-if-desired',con,'*gen-stream*',switches,context,bp],['gs-end-sentence','*gen-stream*']]]],[['switches-lookup',[quote,backtrack],switches],['gs-string-write','*gen-stream*','$STRING'(" no")],[setq,'gened?',t],['gs-end-sentence','*gen-stream*']],[else,[if,[setq,'gened?',[gen,con,'*gen-stream*',switches,context,bp]],[progn,['justify-if-desired',con,'*gen-stream*',switches,context,bp],[if,['ty$instance?',con,[quote,surprise]],['gs-end-exclam','*gen-stream*'],['gs-end-sentence','*gen-stream*']]]]]],['ndbg-end-font','*gate-dbg*',rule],[if,'gened?',[progn,[if,'*typeset?*',[format,'*gate-dbg*','$STRING'("\\vspace{2 mm}~%")]],[if,['null?',['ob$get',context,[quote,'first-gened-concept']]],['ob$set',context,[quote,'first-gened-concept'],con]]]],[if,[and,[],[not,['ty$instance?',con,[quote,surprise]]],[not,['ty$instance?',con,[quote,'overall-emotion']]]],['gen-overall-emot-state']]]])
wl:lambda_def(defun, u_generate1, f_u_generate1, [u_con, u_switches, u_context, u_bp], [[let, [[u_gened_c63, []]], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\vspace{2 mm}~%")]], [u_ndbg_large_bold_font, u_xx_gate_dbg_xx, u_rule], [u_gs_reset_sentence, u_xx_gen_stream_xx], [u_gs_reset_line, u_xx_gen_stream_xx], [if, [and, [<, [u_strength, u_con], u_xx_possibly_realism_xx], [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]], [not, [u_ty_c36_instance_c63, u_con, [quote, u_overall_emotion]]], [not, [u_switches_lookup, [quote, u_tongue_in_cheek], u_switches]]], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " possibly")]], [cond, [[u_switches_lookup, [quote, u_what_if], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " what")], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " if")], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_question, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_tongue_in_cheek], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " anyway,")], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_sentence, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_relaxation], u_switches], [if, [u_performance_mode_c63], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " maybe")], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " say")]], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_sentence, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_backtrack], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " no")], [setq, u_gened_c63, t], [u_gs_end_sentence, u_xx_gen_stream_xx]], [u_else, [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [if, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]], [u_gs_end_exclam, u_xx_gen_stream_xx], [u_gs_end_sentence, u_xx_gen_stream_xx]]]]]], [u_ndbg_end_font, u_xx_gate_dbg_xx, u_rule], [if, u_gened_c63, [progn, [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\vspace{2 mm}~%")]], [if, [u_null_c63, [u_ob_c36_get, u_context, [quote, u_first_gened_concept]]], [u_ob_c36_set, u_context, [quote, u_first_gened_concept], u_con]]]], [if, [and, [], [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]], [not, [u_ty_c36_instance_c63, u_con, [quote, u_overall_emotion]]]], [u_gen_overall_emot_state]]]]).
wl:arglist_info(u_generate1, f_u_generate1, [u_con, u_switches, u_context, u_bp], arginfo{all:[u_con, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_switches, u_context, u_bp], opt:0, req:[u_con, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_generate1).

/*

### Compiled:  `U::GENERATE1` 
*/
f_u_generate1(Con, Switches, Context, Bp, FnResult) :-
	nop(global_env(Env)),
	Env162=[bv(u_con, Con), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	LEnv=[bv(u_gened_c63, [])|Env162],
	get_var(LEnv, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*], claz_base_character, "\\vspace{2 mm}~%")
		      ],
		      TrueResult),
	    _342242662=TrueResult
	;   _342242662=[]
	),
	f_u_ndbg_large_bold_font(u_xx_gate_dbg_xx, u_rule, Rule),
	get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	f_u_gs_reset_sentence(Xx_gen_stream_xx_Get, Reset_sentence_Ret),
	get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get13),
	f_u_gs_reset_line(Xx_gen_stream_xx_Get13, Reset_line_Ret),
	f_u_strength(u_con, PredArg1Result),
	get_var(LEnv, u_xx_possibly_realism_xx, Xx_possibly_realism_xx_Get),
	(   PredArg1Result<Xx_possibly_realism_xx_Get
	->  get_var(LEnv, u_con, Con_Get),
	    f_u_ty_c36_instance_c63(Con_Get, u_surprise, PredArgResult),
	    (   PredArgResult==[]
	    ->  get_var(LEnv, u_con, Con_Get26),
		f_u_ty_c36_instance_c63(Con_Get26,
					u_overall_emotion,
					PredArgResult28),
		(   PredArgResult28==[]
		->  get_var(LEnv, u_switches, Switches_Get),
		    f_u_switches_lookup(u_tongue_in_cheek,
					Switches_Get,
					Not_Param),
		    cl_not(Not_Param, TrueResult30),
		    IFTEST14=TrueResult30
		;   IFTEST14=[]
		)
	    ;   IFTEST14=[]
	    )
	;   IFTEST14=[]
	),
	(   IFTEST14\==[]
	->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get33),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get33,
				'$ARRAY'([*], claz_base_character, " possibly"),
				TrueResult34),
	    _342298980=TrueResult34
	;   _342298980=[]
	),
	get_var(LEnv, u_switches, Switches_Get37),
	f_u_switches_lookup(u_what_if, Switches_Get37, IFTEST35),
	(   IFTEST35\==[]
	->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get38),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get38,
				'$ARRAY'([*], claz_base_character, " what"),
				String_write_Ret),
	    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get39),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get39,
				'$ARRAY'([*], claz_base_character, " if"),
				String_write_Ret174),
	    get_var(LEnv, u_bp, Bp_Get),
	    get_var(LEnv, u_con, Con_Get43),
	    get_var(LEnv, u_context, Context_Get),
	    get_var(LEnv, u_switches, Switches_Get45),
	    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get44),
	    f_u_gen(Con_Get43,
		    Xx_gen_stream_xx_Get44,
		    Switches_Get45,
		    Context_Get,
		    Bp_Get,
		    IFTEST40),
	    set_var(LEnv, u_gened_c63, IFTEST40),
	    (   IFTEST40\==[]
	    ->  get_var(LEnv, u_bp, Bp_Get52),
		get_var(LEnv, u_con, Con_Get48),
		get_var(LEnv, u_context, Context_Get51),
		get_var(LEnv, u_switches, Switches_Get50),
		get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get49),
		f_u_justify_if_desired(Con_Get48,
				       Xx_gen_stream_xx_Get49,
				       Switches_Get50,
				       Context_Get51,
				       Bp_Get52,
				       If_desired_Ret),
		get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get53),
		f_u_gs_end_question(Xx_gen_stream_xx_Get53, TrueResult54),
		TrueResult123=TrueResult54
	    ;   TrueResult123=[]
	    )
	;   get_var(LEnv, u_switches, Switches_Get57),
	    f_u_switches_lookup(u_tongue_in_cheek, Switches_Get57, IFTEST55),
	    (   IFTEST55\==[]
	    ->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get58),
		f_u_gs_string_write(Xx_gen_stream_xx_Get58,
				    '$ARRAY'([*],
					     claz_base_character,
					     " anyway,"),
				    String_write_Ret176),
		get_var(LEnv, u_bp, Bp_Get65),
		get_var(LEnv, u_con, Con_Get61),
		get_var(LEnv, u_context, Context_Get64),
		get_var(LEnv, u_switches, Switches_Get63),
		get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get62),
		f_u_gen(Con_Get61,
			Xx_gen_stream_xx_Get62,
			Switches_Get63,
			Context_Get64,
			Bp_Get65,
			IFTEST59),
		set_var(LEnv, u_gened_c63, IFTEST59),
		(   IFTEST59\==[]
		->  get_var(LEnv, u_bp, Bp_Get70),
		    get_var(LEnv, u_con, Con_Get66),
		    get_var(LEnv, u_context, Context_Get69),
		    get_var(LEnv, u_switches, Switches_Get68),
		    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get67),
		    f_u_justify_if_desired(Con_Get66,
					   Xx_gen_stream_xx_Get67,
					   Switches_Get68,
					   Context_Get69,
					   Bp_Get70,
					   If_desired_Ret177),
		    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get71),
		    f_u_gs_end_sentence(Xx_gen_stream_xx_Get71, TrueResult72),
		    TrueResult123=TrueResult72
		;   TrueResult123=[]
		)
	    ;   get_var(LEnv, u_switches, Switches_Get75),
		f_u_switches_lookup(u_relaxation, Switches_Get75, IFTEST73),
		(   IFTEST73\==[]
		->  f_u_performance_mode_c63(IFTEST76),
		    (   IFTEST76\==[]
		    ->  get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get78),
			f_u_gs_string_write(Xx_gen_stream_xx_Get78,
					    '$ARRAY'([*],
						     claz_base_character,
						     " maybe"),
					    TrueResult80),
			_342640826=TrueResult80
		    ;   get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get79),
			f_u_gs_string_write(Xx_gen_stream_xx_Get79,
					    '$ARRAY'([*],
						     claz_base_character,
						     " say"),
					    ElseResult),
			_342640826=ElseResult
		    ),
		    get_var(LEnv, u_bp, Bp_Get88),
		    get_var(LEnv, u_con, Con_Get84),
		    get_var(LEnv, u_context, Context_Get87),
		    get_var(LEnv, u_switches, Switches_Get86),
		    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get85),
		    f_u_gen(Con_Get84,
			    Xx_gen_stream_xx_Get85,
			    Switches_Get86,
			    Context_Get87,
			    Bp_Get88,
			    IFTEST82),
		    set_var(LEnv, u_gened_c63, IFTEST82),
		    (   IFTEST82\==[]
		    ->  get_var(LEnv, u_bp, Bp_Get93),
			get_var(LEnv, u_con, Con_Get89),
			get_var(LEnv, u_context, Context_Get92),
			get_var(LEnv, u_switches, Switches_Get91),
			get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get90),
			f_u_justify_if_desired(Con_Get89,
					       Xx_gen_stream_xx_Get90,
					       Switches_Get91,
					       Context_Get92,
					       Bp_Get93,
					       If_desired_Ret178),
			get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get94),
			f_u_gs_end_sentence(Xx_gen_stream_xx_Get94,
					    TrueResult95),
			TrueResult123=TrueResult95
		    ;   TrueResult123=[]
		    )
		;   get_var(LEnv, u_switches, Switches_Get98),
		    f_u_switches_lookup(u_backtrack, Switches_Get98, IFTEST96),
		    (   IFTEST96\==[]
		    ->  get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get99),
			f_u_gs_string_write(Xx_gen_stream_xx_Get99,
					    '$ARRAY'([*],
						     claz_base_character,
						     " no"),
					    String_write_Ret179),
			set_var(LEnv, setq, u_gened_c63, t),
			get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get100),
			f_u_gs_end_sentence(Xx_gen_stream_xx_Get100,
					    TrueResult126),
			TrueResult123=TrueResult126
		    ;   get_var(LEnv, u_else, IFTEST101),
			(   IFTEST101\==[]
			->  get_var(LEnv, u_bp, Bp_Get110),
			    get_var(LEnv, u_con, Con_Get106),
			    get_var(LEnv, u_context, Context_Get109),
			    get_var(LEnv, u_switches, Switches_Get108),
			    get_var(LEnv,
				    u_xx_gen_stream_xx,
				    Xx_gen_stream_xx_Get107),
			    f_u_gen(Con_Get106,
				    Xx_gen_stream_xx_Get107,
				    Switches_Get108,
				    Context_Get109,
				    Bp_Get110,
				    IFTEST104),
			    set_var(LEnv, u_gened_c63, IFTEST104),
			    (   IFTEST104\==[]
			    ->  get_var(LEnv, u_bp, Bp_Get115),
				get_var(LEnv, u_con, Con_Get111),
				get_var(LEnv, u_context, Context_Get114),
				get_var(LEnv, u_switches, Switches_Get113),
				get_var(LEnv,
					u_xx_gen_stream_xx,
					Xx_gen_stream_xx_Get112),
				f_u_justify_if_desired(Con_Get111,
						       Xx_gen_stream_xx_Get112,
						       Switches_Get113,
						       Context_Get114,
						       Bp_Get115,
						       If_desired_Ret180),
				get_var(LEnv, u_con, Con_Get118),
				f_u_ty_c36_instance_c63(Con_Get118,
							u_surprise,
							IFTEST116),
				(   IFTEST116\==[]
				->  get_var(LEnv,
					    u_xx_gen_stream_xx,
					    Xx_gen_stream_xx_Get119),
				    f_u_gs_end_exclam(Xx_gen_stream_xx_Get119,
						      TrueResult121),
				    TrueResult123=TrueResult121
				;   get_var(LEnv,
					    u_xx_gen_stream_xx,
					    Xx_gen_stream_xx_Get120),
				    f_u_gs_end_sentence(Xx_gen_stream_xx_Get120,
							ElseResult122),
				    TrueResult123=ElseResult122
				)
			    ;   TrueResult123=[]
			    )
			;   TrueResult123=[]
			)
		    )
		)
	    )
	),
	f_u_ndbg_end_font(u_xx_gate_dbg_xx, u_rule, Rule168),
	get_var(LEnv, u_gened_c63, IFTEST134),
	(   IFTEST134\==[]
	->  get_var(LEnv, u_xx_typeset_c63_xx, IFTEST137),
	    (   IFTEST137\==[]
	    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get140),
		cl_format(
			  [ Xx_gate_dbg_xx_Get140,
			    '$ARRAY'([*],
				     claz_base_character,
				     "\\vspace{2 mm}~%")
			  ],
			  TrueResult141),
		_343009442=TrueResult141
	    ;   _343009442=[]
	    ),
	    f_u_null_c63(
			 [ u_ob_c36_get,
			   u_context,
			   [quote, u_first_gened_concept]
			 ],
			 IFTEST142),
	    (   IFTEST142\==[]
	    ->  get_var(LEnv, u_con, Con_Get145),
		get_var(LEnv, u_context, Context_Get144),
		f_u_ob_c36_set(Context_Get144,
			       u_first_gened_concept,
			       Con_Get145,
			       TrueResult146),
		TrueResult147=TrueResult146
	    ;   TrueResult147=[]
	    )
	;   TrueResult147=[]
	),
	(   []\==[]
	->  get_var(LEnv, u_con, Con_Get153),
	    f_u_ty_c36_instance_c63(Con_Get153, u_surprise, PredArgResult155),
	    (   PredArgResult155==[]
	    ->  get_var(LEnv, u_con, Con_Get156),
		f_u_ty_c36_instance_c63(Con_Get156,
					u_overall_emotion,
					Overall_emotion),
		cl_not(Overall_emotion, TrueResult157),
		IFTEST148=TrueResult157
	    ;   IFTEST148=[]
	    )
	;   IFTEST148=[]
	),
	(   IFTEST148\==[]
	->  f_u_gen_overall_emot_state(TrueResult159),
	    FnResult=TrueResult159
	;   FnResult=[]
	).
:- set_opv(f_u_generate1, classof, claz_function),
   set_opv(u_generate1, compile_as, kw_function),
   set_opv(u_generate1, function, f_u_generate1),
   DefunResult=u_generate1.
/*
:- side_effect(assert_lsp(u_generate1,
			  wl:lambda_def(defun, u_generate1, f_u_generate1, [u_con, u_switches, u_context, u_bp], [[let, [[u_gened_c63, []]], [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\vspace{2 mm}~%")]], [u_ndbg_large_bold_font, u_xx_gate_dbg_xx, u_rule], [u_gs_reset_sentence, u_xx_gen_stream_xx], [u_gs_reset_line, u_xx_gen_stream_xx], [if, [and, [<, [u_strength, u_con], u_xx_possibly_realism_xx], [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]], [not, [u_ty_c36_instance_c63, u_con, [quote, u_overall_emotion]]], [not, [u_switches_lookup, [quote, u_tongue_in_cheek], u_switches]]], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " possibly")]], [cond, [[u_switches_lookup, [quote, u_what_if], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " what")], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " if")], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_question, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_tongue_in_cheek], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " anyway,")], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_sentence, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_relaxation], u_switches], [if, [u_performance_mode_c63], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " maybe")], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " say")]], [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [u_gs_end_sentence, u_xx_gen_stream_xx]]]], [[u_switches_lookup, [quote, u_backtrack], u_switches], [u_gs_string_write, u_xx_gen_stream_xx, '$ARRAY'([*], claz_base_character, " no")], [setq, u_gened_c63, t], [u_gs_end_sentence, u_xx_gen_stream_xx]], [u_else, [if, [setq, u_gened_c63, [u_gen, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp]], [progn, [u_justify_if_desired, u_con, u_xx_gen_stream_xx, u_switches, u_context, u_bp], [if, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]], [u_gs_end_exclam, u_xx_gen_stream_xx], [u_gs_end_sentence, u_xx_gen_stream_xx]]]]]], [u_ndbg_end_font, u_xx_gate_dbg_xx, u_rule], [if, u_gened_c63, [progn, [if, u_xx_typeset_c63_xx, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "\\vspace{2 mm}~%")]], [if, [u_null_c63, [u_ob_c36_get, u_context, [quote, u_first_gened_concept]]], [u_ob_c36_set, u_context, [quote, u_first_gened_concept], u_con]]]], [if, [and, [], [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]], [not, [u_ty_c36_instance_c63, u_con, [quote, u_overall_emotion]]]], [u_gen_overall_emot_state]]]]))).
*/
/*
:- side_effect(assert_lsp(u_generate1,
			  wl:arglist_info(u_generate1, f_u_generate1, [u_con, u_switches, u_context, u_bp], arginfo{all:[u_con, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, u_switches, u_context, u_bp], opt:0, req:[u_con, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_generate1, wl:init_args(exact_only, f_u_generate1))).
*/
/*
   (setq *gen-history* (cons (list con switches context bp) *gen-history*))
*/
/*
     (gs-end-sentence *gen-stream*)
*/
/*
     (gs-string-write *gen-stream* " instead of")
*/
/*
     (setq gened? (gen con *gen-stream* switches context bp))
*/
/*
     (gs-string-write *gen-stream* ", how about this")
*/
/*
     ; A justification will never be used here.
*/
/*
 (ty$instance? con 'emotion) No more overall gen
*/
/*
(defun justify-if-desired (con stream switches context bp)
  (if (switches-lookup 'justify? switches)
      (justify con stream switches context bp)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:6922 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'justify-if-desired',[con,stream,switches,context,bp],[if,['switches-lookup',[quote,'justify?'],switches],[justify,con,stream,switches,context,bp]]])
wl:lambda_def(defun, u_justify_if_desired, f_u_justify_if_desired, [u_con, stream, u_switches, u_context, u_bp], [[if, [u_switches_lookup, [quote, u_justify_c63], u_switches], [u_justify, u_con, stream, u_switches, u_context, u_bp]]]).
wl:arglist_info(u_justify_if_desired, f_u_justify_if_desired, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_justify_if_desired).

/*

### Compiled:  `U::JUSTIFY-IF-DESIRED` 
*/
f_u_justify_if_desired(Con, Stream, Switches, Context, Bp, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_con, Con), bv(stream, Stream), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	get_var(Env15, u_switches, Switches_Get),
	f_u_switches_lookup(u_justify_c63, Switches_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env15, stream, Stream_Get),
	    get_var(Env15, u_bp, Bp_Get),
	    get_var(Env15, u_con, Con_Get),
	    get_var(Env15, u_context, Context_Get),
	    get_var(Env15, u_switches, Switches_Get9),
	    f_u_justify(Con_Get,
			Stream_Get,
			Switches_Get9,
			Context_Get,
			Bp_Get,
			TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_justify_if_desired, classof, claz_function),
   set_opv(u_justify_if_desired, compile_as, kw_function),
   set_opv(u_justify_if_desired, function, f_u_justify_if_desired),
   DefunResult=u_justify_if_desired.
/*
:- side_effect(assert_lsp(u_justify_if_desired,
			  wl:lambda_def(defun, u_justify_if_desired, f_u_justify_if_desired, [u_con, stream, u_switches, u_context, u_bp], [[if, [u_switches_lookup, [quote, u_justify_c63], u_switches], [u_justify, u_con, stream, u_switches, u_context, u_bp]]]))).
*/
/*
:- side_effect(assert_lsp(u_justify_if_desired,
			  wl:arglist_info(u_justify_if_desired, f_u_justify_if_desired, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_justify_if_desired,
			  wl:init_args(exact_only, f_u_justify_if_desired))).
*/
/*
(defun justify (con stream switches context bp)
  (let ((causes 
         (prune
          (get-leaf-causes con context)
          (lambda (x) (ob$ty x)))))
       (if (and causes (neq? (car causes) con))
           (progn
            (gs-string-write stream " because")
            (generate-list causes stream switches context bp)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7074 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,justify,[con,stream,switches,context,bp],[let,[[causes,[prune,['get-leaf-causes',con,context],[lambda,[x],['ob$ty',x]]]]],[if,[and,causes,['neq?',[car,causes],con]],[progn,['gs-string-write',stream,'$STRING'(" because")],['generate-list',causes,stream,switches,context,bp]]]]])
wl:lambda_def(defun, u_justify, f_u_justify, [u_con, stream, u_switches, u_context, u_bp], [[let, [[u_causes, [u_prune, [u_get_leaf_causes, u_con, u_context], [lambda, [u_x], [u_ob_c36_ty, u_x]]]]], [if, [and, u_causes, [u_neq_c63, [car, u_causes], u_con]], [progn, [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " because")], [u_generate_list, u_causes, stream, u_switches, u_context, u_bp]]]]]).
wl:arglist_info(u_justify, f_u_justify, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_justify).

/*

### Compiled:  `U::JUSTIFY` 
*/
f_u_justify(Con, Stream, Switches, Context, Bp, FnResult) :-
	nop(global_env(Env)),
	Env28=[bv(u_con, Con), bv(stream, Stream), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	get_var(Env28, u_con, Con_Get),
	get_var(Env28, u_context, Context_Get),
	f_u_get_leaf_causes(Con_Get, Context_Get, Prune_Param),
	Lambda=closure([ClosureEnvironment|Env28], LResult, [u_x], f_u_ob_c36_ty(u_x, LResult)),
	f_u_prune(Prune_Param, Lambda, Causes_Init),
	LEnv=[bv(u_causes, Causes_Init)|Env28],
	get_var(LEnv, u_causes, IFTEST15),
	(   IFTEST15\==[]
	->  f_u_neq_c63([car, u_causes], u_con, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, stream, Stream_Get),
	    f_u_gs_string_write(Stream_Get,
				'$ARRAY'([*], claz_base_character, " because"),
				String_write_Ret),
	    get_var(LEnv, stream, Stream_Get21),
	    get_var(LEnv, u_bp, Bp_Get),
	    get_var(LEnv, u_causes, Causes_Get20),
	    get_var(LEnv, u_context, Context_Get23),
	    get_var(LEnv, u_switches, Switches_Get),
	    f_u_generate_list(Causes_Get20,
			      Stream_Get21,
			      Switches_Get,
			      Context_Get23,
			      Bp_Get,
			      TrueResult25),
	    FnResult=TrueResult25
	;   FnResult=[]
	).
:- set_opv(f_u_justify, classof, claz_function),
   set_opv(u_justify, compile_as, kw_function),
   set_opv(u_justify, function, f_u_justify),
   DefunResult=u_justify.
/*
:- side_effect(assert_lsp(u_justify,
			  wl:lambda_def(defun, u_justify, f_u_justify, [u_con, stream, u_switches, u_context, u_bp], [[let, [[u_causes, [u_prune, [u_get_leaf_causes, u_con, u_context], [lambda, [u_x], [u_ob_c36_ty, u_x]]]]], [if, [and, u_causes, [u_neq_c63, [car, u_causes], u_con]], [progn, [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " because")], [u_generate_list, u_causes, stream, u_switches, u_context, u_bp]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_justify,
			  wl:arglist_info(u_justify, f_u_justify, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_justify, wl:init_args(exact_only, f_u_justify))).
*/
/*
(defun generate-list (lst stream switches context bp)
  (yloop
   (ywhile lst)
   (ydo (gen (car lst) stream switches context bp)
       (setq lst (cdr lst))
       (if lst (gs-string-write stream " and")))))

;(use-font *dd-output* (load-font *dd-output* "/sys/dm/fonts/std.19l"))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7412 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'generate-list',[lst,stream,switches,context,bp],[yloop,[ywhile,lst],[ydo,[gen,[car,lst],stream,switches,context,bp],[setq,lst,[cdr,lst]],[if,lst,['gs-string-write',stream,'$STRING'(" and")]]]]])
wl:lambda_def(defun, u_generate_list, f_u_generate_list, [u_lst, stream, u_switches, u_context, u_bp], [[u_yloop, [u_ywhile, u_lst], [u_ydo, [u_gen, [car, u_lst], stream, u_switches, u_context, u_bp], [setq, u_lst, [cdr, u_lst]], [if, u_lst, [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")]]]]]).
wl:arglist_info(u_generate_list, f_u_generate_list, [u_lst, stream, u_switches, u_context, u_bp], arginfo{all:[u_lst, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, stream, u_switches, u_context, u_bp], opt:0, req:[u_lst, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_generate_list).

/*

### Compiled:  `U::GENERATE-LIST` 
*/
f_u_generate_list(Lst, Stream, Switches, Context, Bp, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_lst, Lst), bv(stream, Stream), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	f_u_yloop(
		  [ [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      [u_gen, [car, u_lst], stream, u_switches, u_context, u_bp],
		      [setq, u_lst, [cdr, u_lst]],
		      
		      [ if,
			u_lst,
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*], claz_base_character, " and")
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_generate_list, classof, claz_function),
   set_opv(u_generate_list, compile_as, kw_function),
   set_opv(u_generate_list, function, f_u_generate_list),
   DefunResult=u_generate_list.
/*
:- side_effect(assert_lsp(u_generate_list,
			  wl:lambda_def(defun, u_generate_list, f_u_generate_list, [u_lst, stream, u_switches, u_context, u_bp], [[u_yloop, [u_ywhile, u_lst], [u_ydo, [u_gen, [car, u_lst], stream, u_switches, u_context, u_bp], [setq, u_lst, [cdr, u_lst]], [if, u_lst, [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_generate_list,
			  wl:arglist_info(u_generate_list, f_u_generate_list, [u_lst, stream, u_switches, u_context, u_bp], arginfo{all:[u_lst, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, stream, u_switches, u_context, u_bp], opt:0, req:[u_lst, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_generate_list,
			  wl:init_args(exact_only, f_u_generate_list))).
*/
/*
(use-font *dd-output* (load-font *dd-output* "/sys/dm/fonts/std.19l"))
*/
/*
(defun name-is? (ob name)
  (and (ob? ob) (eq? name (ob$name ob))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7695 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'name-is?',[ob,name],[and,['ob?',ob],['eq?',name,['ob$name',ob]]]])
wl:lambda_def(defun, u_name_is_c63, f_u_name_is_c63, [u_ob, sys_name], [[and, [u_ob_c63, u_ob], [u_eq_c63, sys_name, [u_ob_c36_name, u_ob]]]]).
wl:arglist_info(u_name_is_c63, f_u_name_is_c63, [u_ob, sys_name], arginfo{all:[u_ob, sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, sys_name], opt:0, req:[u_ob, sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_name_is_c63).

/*

### Compiled:  `U::NAME-IS?` 
*/
f_u_name_is_c63(Ob, Name, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_ob, Ob), bv(sys_name, Name)|Env],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(sys_name, [u_ob_c36_name, u_ob], TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_name_is_c63, classof, claz_function),
   set_opv(u_name_is_c63, compile_as, kw_function),
   set_opv(u_name_is_c63, function, f_u_name_is_c63),
   DefunResult=u_name_is_c63.
/*
:- side_effect(assert_lsp(u_name_is_c63,
			  wl:lambda_def(defun, u_name_is_c63, f_u_name_is_c63, [u_ob, sys_name], [[and, [u_ob_c63, u_ob], [u_eq_c63, sys_name, [u_ob_c36_name, u_ob]]]]))).
*/
/*
:- side_effect(assert_lsp(u_name_is_c63,
			  wl:arglist_info(u_name_is_c63, f_u_name_is_c63, [u_ob, sys_name], arginfo{all:[u_ob, sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, sys_name], opt:0, req:[u_ob, sys_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_name_is_c63,
			  wl:init_args(exact_only, f_u_name_is_c63))).
*/
/*
(defun get-gen-proc (type)
  (or (ob$get type 'gen)
      (and (ob$gets type 'isa)
           (get-gen-proc (ob$get type 'isa)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7764 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-gen-proc',[type],[or,['ob$get',type,[quote,gen]],[and,['ob$gets',type,[quote,isa]],['get-gen-proc',['ob$get',type,[quote,isa]]]]]])
wl:lambda_def(defun, u_get_gen_proc, f_u_get_gen_proc, [type], [[or, [u_ob_c36_get, type, [quote, u_gen]], [and, [u_ob_c36_gets, type, [quote, u_isa]], [u_get_gen_proc, [u_ob_c36_get, type, [quote, u_isa]]]]]]).
wl:arglist_info(u_get_gen_proc, f_u_get_gen_proc, [type], arginfo{all:[type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[type], opt:0, req:[type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_gen_proc).

/*

### Compiled:  `U::GET-GEN-PROC` 
*/
f_u_get_gen_proc(Type, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(type, Type)|Env],
	(   get_var(Env13, type, Type_Get),
	    f_u_ob_c36_get(Type_Get, u_gen, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   get_var(Env13, type, Type_Get7),
	    f_u_ob_c36_gets(Type_Get7, u_isa, IFTEST),
	    (   IFTEST\==[]
	    ->  get_var(Env13, type, Type_Get8),
		f_u_ob_c36_get(Type_Get8, u_isa, Isa),
		f_u_get_gen_proc(Isa, TrueResult),
		FnResult=TrueResult
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_get_gen_proc, classof, claz_function),
   set_opv(u_get_gen_proc, compile_as, kw_function),
   set_opv(u_get_gen_proc, function, f_u_get_gen_proc),
   DefunResult=u_get_gen_proc.
/*
:- side_effect(assert_lsp(u_get_gen_proc,
			  wl:lambda_def(defun, u_get_gen_proc, f_u_get_gen_proc, [type], [[or, [u_ob_c36_get, type, [quote, u_gen]], [and, [u_ob_c36_gets, type, [quote, u_isa]], [u_get_gen_proc, [u_ob_c36_get, type, [quote, u_isa]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_get_gen_proc,
			  wl:arglist_info(u_get_gen_proc, f_u_get_gen_proc, [type], arginfo{all:[type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[type], opt:0, req:[type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_gen_proc,
			  wl:init_args(exact_only, f_u_get_gen_proc))).
*/
/*
(defun gen (con stream switches context bp)
 (prog1
  (cond
   ((var? con)
    (gen-variable con stream switches context bp))
   ((ob? con)
    (let ((gen-proc (get-gen-proc (ob$ty con))))
      (cond
       ((eq? gen-proc 'no-gen) nil) ; here we wanted to flush up to top level
       (gen-proc
         (funcall gen-proc con stream switches context bp))
       (else (gen-unknown con stream)))))
   ((and (pair? con) (null? (cdr con)))
    (gen (car con) stream switches context bp))
   ((pair? con)
    (if (memq? *me-ob* con)
        (progn
         (yloop (initial (others (non-mes con)))
               (ywhile others)
               (ydo (gen (car others) stream switches context bp)
                   (setq others (cdr others))))
         (gs-string-write stream " and")
         (gen *me-ob* stream switches context bp))
        (yloop (ywhile con)
              (ydo (gen (car con) stream switches context bp)
                  (setq con (cdr con))
                  (if (and con
                           (null? (cdr con)))
                      (gs-string-write stream " and")))))
     con)
   (else (gen-unknown con stream)))
  (if (and (ob? con)
           (ty$instance? con 'object)
           ; For now, the below prevents "I want to be
           ; going out with this person." Originally,
           ; this was used for action mutation generation.
           (not (exemplar? con))
           (neq? con *me-ob*)
           (not (memq? con *references*)))
          (setq *references*
           (cons con (del-old-ref *references*
                                  con))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,gen,[con,stream,switches,context,bp],[prog1,[cond,[['var?',con],['gen-variable',con,stream,switches,context,bp]],[['ob?',con],[let,[['gen-proc',['get-gen-proc',['ob$ty',con]]]],[cond,[['eq?','gen-proc',[quote,'no-gen']],[]],['gen-proc',[funcall,'gen-proc',con,stream,switches,context,bp]],[else,['gen-unknown',con,stream]]]]],[[and,['pair?',con],['null?',[cdr,con]]],[gen,[car,con],stream,switches,context,bp]],[['pair?',con],[if,['memq?','*me-ob*',con],[progn,[yloop,[initial,[others,['non-mes',con]]],[ywhile,others],[ydo,[gen,[car,others],stream,switches,context,bp],[setq,others,[cdr,others]]]],['gs-string-write',stream,'$STRING'(" and")],[gen,'*me-ob*',stream,switches,context,bp]],[yloop,[ywhile,con],[ydo,[gen,[car,con],stream,switches,context,bp],[setq,con,[cdr,con]],[if,[and,con,['null?',[cdr,con]]],['gs-string-write',stream,'$STRING'(" and")]]]]],con],[else,['gen-unknown',con,stream]]],[if,[and,['ob?',con],['ty$instance?',con,[quote,object]],[not,['exemplar?',con]],['neq?',con,'*me-ob*'],[not,['memq?',con,'*references*']]],[setq,'*references*',[cons,con,['del-old-ref','*references*',con]]]]]])
wl:lambda_def(defun, u_gen, f_u_gen, [u_con, stream, u_switches, u_context, u_bp], [[prog1, [cond, [[u_var_c63, u_con], [u_gen_variable, u_con, stream, u_switches, u_context, u_bp]], [[u_ob_c63, u_con], [let, [[u_gen_proc, [u_get_gen_proc, [u_ob_c36_ty, u_con]]]], [cond, [[u_eq_c63, u_gen_proc, [quote, u_no_gen]], []], [u_gen_proc, [funcall, u_gen_proc, u_con, stream, u_switches, u_context, u_bp]], [u_else, [u_gen_unknown, u_con, stream]]]]], [[and, [u_pair_c63, u_con], [u_null_c63, [cdr, u_con]]], [u_gen, [car, u_con], stream, u_switches, u_context, u_bp]], [[u_pair_c63, u_con], [if, [u_memq_c63, u_xx_me_ob_xx, u_con], [progn, [u_yloop, [u_initial, [u_others, [u_non_mes, u_con]]], [u_ywhile, u_others], [u_ydo, [u_gen, [car, u_others], stream, u_switches, u_context, u_bp], [setq, u_others, [cdr, u_others]]]], [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")], [u_gen, u_xx_me_ob_xx, stream, u_switches, u_context, u_bp]], [u_yloop, [u_ywhile, u_con], [u_ydo, [u_gen, [car, u_con], stream, u_switches, u_context, u_bp], [setq, u_con, [cdr, u_con]], [if, [and, u_con, [u_null_c63, [cdr, u_con]]], [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")]]]]], u_con], [u_else, [u_gen_unknown, u_con, stream]]], [if, [and, [u_ob_c63, u_con], [u_ty_c36_instance_c63, u_con, [quote, u_object]], [not, [u_exemplar_c63, u_con]], [u_neq_c63, u_con, u_xx_me_ob_xx], [not, [u_memq_c63, u_con, u_xx_references_xx]]], [setq, u_xx_references_xx, [cons, u_con, [u_del_old_ref, u_xx_references_xx, u_con]]]]]]).
wl:arglist_info(u_gen, f_u_gen, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gen).

/*

### Compiled:  `U::GEN` 
*/
f_u_gen(Con, Stream, Switches, Context, Bp, ElseResult36) :-
	nop(global_env(Env)),
	AEnv=[bv(u_con, Con), bv(stream, Stream), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	f_u_var_c63(u_con, IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, stream, Stream_Get),
	    get_var(AEnv, u_bp, Bp_Get),
	    get_var(AEnv, u_con, Con_Get),
	    get_var(AEnv, u_context, Context_Get),
	    get_var(AEnv, u_switches, Switches_Get),
	    f_u_gen_variable(Con_Get,
			     Stream_Get,
			     Switches_Get,
			     Context_Get,
			     Bp_Get,
			     TrueResult74),
	    ElseResult36=TrueResult74
	;   f_u_ob_c63(u_con, IFTEST11),
	    (   IFTEST11\==[]
	    ->  f_u_ob_c36_ty(u_con, Gen_proc_Param),
		f_u_get_gen_proc(Gen_proc_Param, Gen_proc_Init),
		LEnv=[bv(u_gen_proc, Gen_proc_Init)|AEnv],
		f_u_eq_c63(u_gen_proc, [quote, u_no_gen], IFTEST17),
		(   IFTEST17\==[]
		->  ElseResult36=[]
		;   get_var(LEnv, u_gen_proc, IFTEST19),
		    (   IFTEST19\==[]
		    ->  get_var(LEnv, stream, Stream_Get24),
			get_var(LEnv, u_bp, Bp_Get27),
			get_var(LEnv, u_con, Con_Get23),
			get_var(LEnv, u_context, Context_Get26),
			get_var(LEnv, u_gen_proc, Gen_proc_Get22),
			get_var(LEnv, u_switches, Switches_Get25),
			cl_apply(Gen_proc_Get22,
				 
				 [ Con_Get23,
				   Stream_Get24,
				   Switches_Get25,
				   Context_Get26,
				   Bp_Get27
				 ],
				 TrueResult35),
			ElseResult36=TrueResult35
		    ;   get_var(LEnv, u_else, IFTEST28),
			(   IFTEST28\==[]
			->  get_var(LEnv, stream, Stream_Get32),
			    get_var(LEnv, u_con, Con_Get31),
			    f_u_gen_unknown(Con_Get31, Stream_Get32, TrueResult),
			    ElseResult36=TrueResult
			;   ElseResult36=[]
			)
		    )
		)
	    ;   f_u_pair_c63(u_con, IFTEST40),
		(   IFTEST40\==[]
		->  f_u_null_c63([cdr, u_con], TrueResult42),
		    IFTEST38=TrueResult42
		;   IFTEST38=[]
		),
		(   IFTEST38\==[]
		->  get_var(AEnv, u_con, Con_Get43),
		    cl_car(Con_Get43, Gen_Param),
		    get_var(AEnv, stream, Stream_Get44),
		    get_var(AEnv, u_bp, Bp_Get47),
		    get_var(AEnv, u_context, Context_Get46),
		    get_var(AEnv, u_switches, Switches_Get45),
		    f_u_gen(Gen_Param,
			    Stream_Get44,
			    Switches_Get45,
			    Context_Get46,
			    Bp_Get47,
			    TrueResult70),
		    ElseResult36=TrueResult70
		;   f_u_pair_c63(u_con, IFTEST48),
		    (   IFTEST48\==[]
		    ->  f_u_memq_c63(u_xx_me_ob_xx, u_con, IFTEST50),
			(   IFTEST50\==[]
			->  f_u_yloop(
				      [ 
					[ u_initial,
					  [u_others, [u_non_mes, u_con]]
					],
					[u_ywhile, u_others],
					
					[ u_ydo,
					  
					  [ u_gen,
					    [car, u_others],
					    stream,
					    u_switches,
					    u_context,
					    u_bp
					  ],
					  [setq, u_others, [cdr, u_others]]
					]
				      ],
				      Yloop_Ret),
			    get_var(AEnv, stream, Stream_Get52),
			    f_u_gs_string_write(Stream_Get52,
						'$ARRAY'([*],
							 claz_base_character,
							 " and"),
						String_write_Ret),
			    get_var(AEnv, stream, Stream_Get54),
			    get_var(AEnv, u_bp, Bp_Get57),
			    get_var(AEnv, u_context, Context_Get56),
			    get_var(AEnv, u_switches, Switches_Get55),
			    get_var(AEnv, u_xx_me_ob_xx, Xx_me_ob_xx_Get),
			    f_u_gen(Xx_me_ob_xx_Get,
				    Stream_Get54,
				    Switches_Get55,
				    Context_Get56,
				    Bp_Get57,
				    TrueResult58),
			    _362165928=TrueResult58
			;   f_u_yloop(
				      [ [u_ywhile, u_con],
					
					[ u_ydo,
					  
					  [ u_gen,
					    [car, u_con],
					    stream,
					    u_switches,
					    u_context,
					    u_bp
					  ],
					  [setq, u_con, [cdr, u_con]],
					  
					  [ if,
					    
					    [ and,
					      u_con,
					      [u_null_c63, [cdr, u_con]]
					    ],
					    
					    [ u_gs_string_write,
					      stream,
					      '$ARRAY'([*],
						       claz_base_character,
						       " and")
					    ]
					  ]
					]
				      ],
				      ElseResult59),
			    _362165928=ElseResult59
			),
			get_var(AEnv, u_con, Con_Get60),
			ElseResult36=Con_Get60
		    ;   get_var(AEnv, u_else, IFTEST61),
			(   IFTEST61\==[]
			->  get_var(AEnv, stream, Stream_Get65),
			    get_var(AEnv, u_con, Con_Get64),
			    f_u_gen_unknown(Con_Get64,
					    Stream_Get65,
					    TrueResult66),
			    ElseResult36=TrueResult66
			;   ElseResult36=[]
			)
		    )
		)
	    )
	),
	f_u_ob_c63(u_con, IFTEST78),
	(   IFTEST78\==[]
	->  get_var(AEnv, u_con, Con_Get82),
	    f_u_ty_c36_instance_c63(Con_Get82, u_object, IFTEST80),
	    (   IFTEST80\==[]
	    ->  get_var(AEnv, u_con, Con_Get84),
		f_u_exemplar_c63(Con_Get84, PredArgResult),
		(   PredArgResult==[]
		->  f_u_neq_c63(u_con, u_xx_me_ob_xx, IFTEST87),
		    (   IFTEST87\==[]
		    ->  f_u_memq_c63(u_con,
				     u_xx_references_xx,
				     Xx_references_xx),
			cl_not(Xx_references_xx, TrueResult89),
			IFTEST76=TrueResult89
		    ;   IFTEST76=[]
		    )
		;   IFTEST76=[]
		)
	    ;   IFTEST76=[]
	    )
	;   IFTEST76=[]
	),
	(   IFTEST76\==[]
	->  get_var(AEnv, u_con, Con_Get94),
	    get_var(AEnv, u_xx_references_xx, Xx_references_xx_Get),
	    f_u_del_old_ref(Xx_references_xx_Get, Con_Get94, Old_ref_Ret),
	    TrueResult97=[Con_Get94|Old_ref_Ret],
	    set_var(AEnv, u_xx_references_xx, TrueResult97),
	    _362369702=TrueResult97
	;   _362369702=[]
	).
:- set_opv(f_u_gen, classof, claz_function),
   set_opv(u_gen, compile_as, kw_function),
   set_opv(u_gen, function, f_u_gen),
   DefunResult=u_gen.
/*
:- side_effect(assert_lsp(u_gen,
			  wl:lambda_def(defun, u_gen, f_u_gen, [u_con, stream, u_switches, u_context, u_bp], [[prog1, [cond, [[u_var_c63, u_con], [u_gen_variable, u_con, stream, u_switches, u_context, u_bp]], [[u_ob_c63, u_con], [let, [[u_gen_proc, [u_get_gen_proc, [u_ob_c36_ty, u_con]]]], [cond, [[u_eq_c63, u_gen_proc, [quote, u_no_gen]], []], [u_gen_proc, [funcall, u_gen_proc, u_con, stream, u_switches, u_context, u_bp]], [u_else, [u_gen_unknown, u_con, stream]]]]], [[and, [u_pair_c63, u_con], [u_null_c63, [cdr, u_con]]], [u_gen, [car, u_con], stream, u_switches, u_context, u_bp]], [[u_pair_c63, u_con], [if, [u_memq_c63, u_xx_me_ob_xx, u_con], [progn, [u_yloop, [u_initial, [u_others, [u_non_mes, u_con]]], [u_ywhile, u_others], [u_ydo, [u_gen, [car, u_others], stream, u_switches, u_context, u_bp], [setq, u_others, [cdr, u_others]]]], [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")], [u_gen, u_xx_me_ob_xx, stream, u_switches, u_context, u_bp]], [u_yloop, [u_ywhile, u_con], [u_ydo, [u_gen, [car, u_con], stream, u_switches, u_context, u_bp], [setq, u_con, [cdr, u_con]], [if, [and, u_con, [u_null_c63, [cdr, u_con]]], [u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " and")]]]]], u_con], [u_else, [u_gen_unknown, u_con, stream]]], [if, [and, [u_ob_c63, u_con], [u_ty_c36_instance_c63, u_con, [quote, u_object]], [not, [u_exemplar_c63, u_con]], [u_neq_c63, u_con, u_xx_me_ob_xx], [not, [u_memq_c63, u_con, u_xx_references_xx]]], [setq, u_xx_references_xx, [cons, u_con, [u_del_old_ref, u_xx_references_xx, u_con]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_gen,
			  wl:arglist_info(u_gen, f_u_gen, [u_con, stream, u_switches, u_context, u_bp], arginfo{all:[u_con, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream, u_switches, u_context, u_bp], opt:0, req:[u_con, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gen, wl:init_args(exact_only, f_u_gen))).
*/
/*
 here we wanted to flush up to top level
*/
/*
 For now, the below prevents "I want to be
*/
/*
 going out with this person." Originally,
*/
/*
 this was used for action mutation generation.
*/
/*
(defun del-type (lst typ)
  (yloop (initial (result nil))
        (yfor item in lst)
        (ydo (if (neq? (ob$ty item) typ)
                (setq result (cons item result))))
        (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:9491 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'del-type',[lst,typ],[yloop,[initial,[result,[]]],[yfor,item,in,lst],[ydo,[if,['neq?',['ob$ty',item],typ],[setq,result,[cons,item,result]]]],[yresult,result]]])
wl:lambda_def(defun, u_del_type, f_u_del_type, [u_lst, u_typ], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, item, u_in, u_lst], [u_ydo, [if, [u_neq_c63, [u_ob_c36_ty, item], u_typ], [setq, u_result, [cons, item, u_result]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_del_type, f_u_del_type, [u_lst, u_typ], arginfo{all:[u_lst, u_typ], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, u_typ], opt:0, req:[u_lst, u_typ], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_del_type).

/*

### Compiled:  `U::DEL-TYPE` 
*/
f_u_del_type(Lst, Typ, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_lst, Lst), bv(u_typ, Typ)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, item, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_neq_c63, [u_ob_c36_ty, item], u_typ],
			[setq, u_result, [cons, item, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_del_type, classof, claz_function),
   set_opv(u_del_type, compile_as, kw_function),
   set_opv(u_del_type, function, f_u_del_type),
   DefunResult=u_del_type.
/*
:- side_effect(assert_lsp(u_del_type,
			  wl:lambda_def(defun, u_del_type, f_u_del_type, [u_lst, u_typ], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, item, u_in, u_lst], [u_ydo, [if, [u_neq_c63, [u_ob_c36_ty, item], u_typ], [setq, u_result, [cons, item, u_result]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_del_type,
			  wl:arglist_info(u_del_type, f_u_del_type, [u_lst, u_typ], arginfo{all:[u_lst, u_typ], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, u_typ], opt:0, req:[u_lst, u_typ], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_del_type, wl:init_args(exact_only, f_u_del_type))).
*/
/*
(defun del-old-ref (lst con)
  (let ((typ (cond
              ((ty$instance? con 'male-person)
               *male-person-ob*)
              ((ty$instance? con 'female-person)
               *female-person-ob*)
              (else nil))))
     (if typ
         (yloop (initial (result nil))
               (yfor item in lst)
               (ydo (if (not (ty$instance-of? item typ))
                       (setq result (cons item result))))
               (yresult result))
         (del-type lst (ob$ty con)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:9696 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'del-old-ref',[lst,con],[let,[[typ,[cond,[['ty$instance?',con,[quote,'male-person']],'*male-person-ob*'],[['ty$instance?',con,[quote,'female-person']],'*female-person-ob*'],[else,[]]]]],[if,typ,[yloop,[initial,[result,[]]],[yfor,item,in,lst],[ydo,[if,[not,['ty$instance-of?',item,typ]],[setq,result,[cons,item,result]]]],[yresult,result]],['del-type',lst,['ob$ty',con]]]]])
wl:lambda_def(defun, u_del_old_ref, f_u_del_old_ref, [u_lst, u_con], [[let, [[u_typ, [cond, [[u_ty_c36_instance_c63, u_con, [quote, u_male_person]], u_xx_male_person_ob_xx], [[u_ty_c36_instance_c63, u_con, [quote, u_female_person]], u_xx_female_person_ob_xx], [u_else, []]]]], [if, u_typ, [u_yloop, [u_initial, [u_result, []]], [u_yfor, item, u_in, u_lst], [u_ydo, [if, [not, [u_ty_c36_instance_of_c63, item, u_typ]], [setq, u_result, [cons, item, u_result]]]], [u_yresult, u_result]], [u_del_type, u_lst, [u_ob_c36_ty, u_con]]]]]).
wl:arglist_info(u_del_old_ref, f_u_del_old_ref, [u_lst, u_con], arginfo{all:[u_lst, u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, u_con], opt:0, req:[u_lst, u_con], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_del_old_ref).

/*

### Compiled:  `U::DEL-OLD-REF` 
*/
f_u_del_old_ref(Lst, Con, FnResult) :-
	nop(global_env(Env)),
	Env32=[bv(u_lst, Lst), bv(u_con, Con)|Env],
	get_var(Env32, u_con, Con_Get),
	f_u_ty_c36_instance_c63(Con_Get, u_male_person, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env32, u_xx_male_person_ob_xx, Xx_male_person_ob_xx_Get),
	    ElseResult20=Xx_male_person_ob_xx_Get
	;   get_var(Env32, u_con, Con_Get13),
	    f_u_ty_c36_instance_c63(Con_Get13, u_female_person, IFTEST11),
	    (   IFTEST11\==[]
	    ->  get_var(Env32,
			u_xx_female_person_ob_xx,
			Xx_female_person_ob_xx_Get),
		ElseResult20=Xx_female_person_ob_xx_Get
	    ;   get_var(Env32, u_else, IFTEST15),
		(   IFTEST15\==[]
		->  ElseResult20=[]
		;   ElseResult20=[]
		)
	    )
	),
	LEnv=[bv(u_typ, ElseResult20)|Env32],
	get_var(LEnv, u_typ, IFTEST24),
	(   IFTEST24\==[]
	->  f_u_yloop(
		      [ [u_initial, [u_result, []]],
			[u_yfor, item, u_in, u_lst],
			
			[ u_ydo,
			  
			  [ if,
			    [not, [u_ty_c36_instance_of_c63, item, u_typ]],
			    [setq, u_result, [cons, item, u_result]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      TrueResult28),
	    FnResult=TrueResult28
	;   get_var(LEnv, u_lst, Lst_Get),
	    f_u_ob_c36_ty(u_con, C36_ty_Ret),
	    f_u_del_type(Lst_Get, C36_ty_Ret, ElseResult29),
	    FnResult=ElseResult29
	).
:- set_opv(f_u_del_old_ref, classof, claz_function),
   set_opv(u_del_old_ref, compile_as, kw_function),
   set_opv(u_del_old_ref, function, f_u_del_old_ref),
   DefunResult=u_del_old_ref.
/*
:- side_effect(assert_lsp(u_del_old_ref,
			  wl:lambda_def(defun, u_del_old_ref, f_u_del_old_ref, [u_lst, u_con], [[let, [[u_typ, [cond, [[u_ty_c36_instance_c63, u_con, [quote, u_male_person]], u_xx_male_person_ob_xx], [[u_ty_c36_instance_c63, u_con, [quote, u_female_person]], u_xx_female_person_ob_xx], [u_else, []]]]], [if, u_typ, [u_yloop, [u_initial, [u_result, []]], [u_yfor, item, u_in, u_lst], [u_ydo, [if, [not, [u_ty_c36_instance_of_c63, item, u_typ]], [setq, u_result, [cons, item, u_result]]]], [u_yresult, u_result]], [u_del_type, u_lst, [u_ob_c36_ty, u_con]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_del_old_ref,
			  wl:arglist_info(u_del_old_ref, f_u_del_old_ref, [u_lst, u_con], arginfo{all:[u_lst, u_con], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst, u_con], opt:0, req:[u_lst, u_con], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_del_old_ref,
			  wl:init_args(exact_only, f_u_del_old_ref))).
*/
/*
(defun gen-unknown (con stream)
  (gs-string-write stream " something")
  (if (not *typeset?*)
      (format stream " ("(defun gen-unknown (con stream)\n  (gs-string-write stream \" something\")\n  (if (not *typeset?*)\n      (format stream \" (~A)\" (if (ob? con)\n                                 (ob$name con)\n                                 con)))\n  t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10210 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'gen-unknown',[con,stream],['gs-string-write',stream,'$STRING'(" something")],[if,[not,'*typeset?*'],[format,stream,'$STRING'(" (~A)"),[if,['ob?',con],['ob$name',con],con]]],t])
wl:lambda_def(defun, u_gen_unknown, f_u_gen_unknown, [u_con, stream], [[u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " something")], [if, [not, u_xx_typeset_c63_xx], [format, stream, '$ARRAY'([*], claz_base_character, " (~A)"), [if, [u_ob_c63, u_con], [u_ob_c36_name, u_con], u_con]]], t]).
wl:arglist_info(u_gen_unknown, f_u_gen_unknown, [u_con, stream], arginfo{all:[u_con, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream], opt:0, req:[u_con, stream], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gen_unknown).

/*

### Compiled:  `U::GEN-UNKNOWN` 
*/
f_u_gen_unknown(Con, Stream, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_con, Con), bv(stream, Stream)|Env],
	get_var(Env19, stream, Stream_Get),
	f_u_gs_string_write(Stream_Get,
			    '$ARRAY'([*], claz_base_character, " something"),
			    String_write_Ret),
	get_var(Env19, u_xx_typeset_c63_xx, Xx_typeset_c63_xx_Get),
	(   Xx_typeset_c63_xx_Get==[]
	->  get_var(Env19, stream, Stream_Get9),
	    f_u_ob_c63(u_con, IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(Env19, u_con, Con_Get),
		f_u_ob_c36_name(Con_Get, TrueResult),
		CAR=TrueResult
	    ;   get_var(Env19, u_con, Con_Get13),
		CAR=Con_Get13
	    ),
	    cl_format(
		      [ Stream_Get9,
			'$ARRAY'([*], claz_base_character, " (~A)"),
			CAR
		      ],
		      TrueResult16),
	    _370848090=TrueResult16
	;   _370848090=[]
	),
	t=FnResult.
:- set_opv(f_u_gen_unknown, classof, claz_function),
   set_opv(u_gen_unknown, compile_as, kw_function),
   set_opv(u_gen_unknown, function, f_u_gen_unknown),
   DefunResult=u_gen_unknown.
/*
:- side_effect(assert_lsp(u_gen_unknown,
			  wl:lambda_def(defun, u_gen_unknown, f_u_gen_unknown, [u_con, stream], [[u_gs_string_write, stream, '$ARRAY'([*], claz_base_character, " something")], [if, [not, u_xx_typeset_c63_xx], [format, stream, '$ARRAY'([*], claz_base_character, " (~A)"), [if, [u_ob_c63, u_con], [u_ob_c36_name, u_con], u_con]]], t]))).
*/
/*
:- side_effect(assert_lsp(u_gen_unknown,
			  wl:arglist_info(u_gen_unknown, f_u_gen_unknown, [u_con, stream], arginfo{all:[u_con, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_con, stream], opt:0, req:[u_con, stream], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gen_unknown,
			  wl:init_args(exact_only, f_u_gen_unknown))).
*/
/*
(defun gen-variable (var stream switches context bp)
  (let ((typ (variable-type var)))
       (if (ob? typ)
           (gen (ob$get typ 'exemplar) stream switches context bp)
           (gen-unknown var stream))
       t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10441 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'gen-variable',[var,stream,switches,context,bp],[let,[[typ,['variable-type',var]]],[if,['ob?',typ],[gen,['ob$get',typ,[quote,exemplar]],stream,switches,context,bp],['gen-unknown',var,stream]],t]])
wl:lambda_def(defun, u_gen_variable, f_u_gen_variable, [u_var, stream, u_switches, u_context, u_bp], [[let, [[u_typ, [u_variable_type, u_var]]], [if, [u_ob_c63, u_typ], [u_gen, [u_ob_c36_get, u_typ, [quote, u_exemplar]], stream, u_switches, u_context, u_bp], [u_gen_unknown, u_var, stream]], t]]).
wl:arglist_info(u_gen_variable, f_u_gen_variable, [u_var, stream, u_switches, u_context, u_bp], arginfo{all:[u_var, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, stream, u_switches, u_context, u_bp], opt:0, req:[u_var, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gen_variable).

/*

### Compiled:  `U::GEN-VARIABLE` 
*/
f_u_gen_variable(Var, Stream, Switches, Context, Bp, FnResult) :-
	nop(global_env(Env)),
	Env21=[bv(u_var, Var), bv(stream, Stream), bv(u_switches, Switches), bv(u_context, Context), bv(u_bp, Bp)|Env],
	f_u_variable_type(u_var, Typ_Init),
	LEnv=[bv(u_typ, Typ_Init)|Env21],
	f_u_ob_c63(u_typ, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_typ, Typ_Get),
	    f_u_ob_c36_get(Typ_Get, u_exemplar, Exemplar),
	    get_var(LEnv, stream, Stream_Get),
	    get_var(LEnv, u_bp, Bp_Get),
	    get_var(LEnv, u_context, Context_Get),
	    get_var(LEnv, u_switches, Switches_Get),
	    f_u_gen(Exemplar,
		    Stream_Get,
		    Switches_Get,
		    Context_Get,
		    Bp_Get,
		    TrueResult),
	    _372326702=TrueResult
	;   get_var(LEnv, stream, Stream_Get16),
	    get_var(LEnv, u_var, Var_Get),
	    f_u_gen_unknown(Var_Get, Stream_Get16, ElseResult),
	    _372326702=ElseResult
	),
	t=FnResult.
:- set_opv(f_u_gen_variable, classof, claz_function),
   set_opv(u_gen_variable, compile_as, kw_function),
   set_opv(u_gen_variable, function, f_u_gen_variable),
   DefunResult=u_gen_variable.
/*
:- side_effect(assert_lsp(u_gen_variable,
			  wl:lambda_def(defun, u_gen_variable, f_u_gen_variable, [u_var, stream, u_switches, u_context, u_bp], [[let, [[u_typ, [u_variable_type, u_var]]], [if, [u_ob_c63, u_typ], [u_gen, [u_ob_c36_get, u_typ, [quote, u_exemplar]], stream, u_switches, u_context, u_bp], [u_gen_unknown, u_var, stream]], t]]))).
*/
/*
:- side_effect(assert_lsp(u_gen_variable,
			  wl:arglist_info(u_gen_variable, f_u_gen_variable, [u_var, stream, u_switches, u_context, u_bp], arginfo{all:[u_var, stream, u_switches, u_context, u_bp], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, stream, u_switches, u_context, u_bp], opt:0, req:[u_var, stream, u_switches, u_context, u_bp], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gen_variable,
			  wl:init_args(exact_only, f_u_gen_variable))).
*/
/*
(defun exemplar? (ob)
  (and (ob$get ob 'type)
       (eq? ob (ob$get (ob$get ob 'type) 'exemplar))))

; Todo:
; - Allow flagging of subgoals not to be generated.
; - Add lookup for locations
; - Return subject in gen to enable NP-deletion below in 'by' phrases.
; - Diff btwn gening, say RPROX infs and RPROXs otherwise, say nested
;   in ep.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'exemplar?',[ob],[and,['ob$get',ob,[quote,type]],['eq?',ob,['ob$get',['ob$get',ob,[quote,type]],[quote,exemplar]]]]])
wl:lambda_def(defun, u_exemplar_c63, f_u_exemplar_c63, [u_ob], [[and, [u_ob_c36_get, u_ob, [quote, type]], [u_eq_c63, u_ob, [u_ob_c36_get, [u_ob_c36_get, u_ob, [quote, type]], [quote, u_exemplar]]]]]).
wl:arglist_info(u_exemplar_c63, f_u_exemplar_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_exemplar_c63).

/*

### Compiled:  `U::EXEMPLAR?` 
*/
f_u_exemplar_c63(Ob, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_ob, Ob)|Env],
	get_var(Env10, u_ob, Ob_Get),
	f_u_ob_c36_get(Ob_Get, type, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_ob,
		       
		       [ u_ob_c36_get,
			 [u_ob_c36_get, u_ob, [quote, type]],
			 [quote, u_exemplar]
		       ],
		       TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_exemplar_c63, classof, claz_function),
   set_opv(u_exemplar_c63, compile_as, kw_function),
   set_opv(u_exemplar_c63, function, f_u_exemplar_c63),
   DefunResult=u_exemplar_c63.
/*
:- side_effect(assert_lsp(u_exemplar_c63,
			  wl:lambda_def(defun, u_exemplar_c63, f_u_exemplar_c63, [u_ob], [[and, [u_ob_c36_get, u_ob, [quote, type]], [u_eq_c63, u_ob, [u_ob_c36_get, [u_ob_c36_get, u_ob, [quote, type]], [quote, u_exemplar]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_exemplar_c63,
			  wl:arglist_info(u_exemplar_c63, f_u_exemplar_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_exemplar_c63,
			  wl:init_args(exact_only, f_u_exemplar_c63))).
*/
/*
 Todo:
*/
/*
 - Allow flagging of subgoals not to be generated.
*/
/*
 - Add lookup for locations
*/
/*
 - Return subject in gen to enable NP-deletion below in 'by' phrases.
*/
/*
 - Diff btwn gening, say RPROX infs and RPROXs otherwise, say nested
*/
/*
   in ep.
*/
/*
(define-gen EPISODE nil
  (let ((goal (ob$get con 'goal)))
     (gen-subgoals goal stream switches context bp t)
     *me-ob*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:11010 **********************/
:-lisp_compile_to_prolog(pkg_user,['define-gen','EPISODE',[],[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']])
:- f_u_define_gen(u_episode,
		  [],
		  
		  [ 
		    [ let,
		      [[u_goal, [u_ob_c36_get, u_con, [quote, u_goal]]]],
		      
		      [ u_gen_subgoals,
			u_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			t
		      ],
		      u_xx_me_ob_xx
		    ]
		  ],
		  _Ignored).
/*
% uncaught(error(existence_error(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375351864))):-rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375057678)),!,fail.
*/
(0)$[system] system:'$c_call_prolog'.  no(clause) 
(1)$[plevel] plevel:'$initialise'.   %  toplevel.pl:489: 
(2)$[system] system:catch(plevel:initialise_prolog,_375660380,plevel:initialise_error(_375660380)).   %  init.pl:371: 
(3)$[plevel] plevel:initialise_prolog.   %  toplevel.pl:497: 
(4)$[plevel] plevel:run_main_init.   %  toplevel.pl:562: 
(5)*$[plevel] plevel:run_init_goal(cl:lisp,@(cl:lisp,'epl.pl':243)).   %  toplevel.pl:573: 
(6)$[system] system:catch(cl:lisp,_375669868,plevel:true).   %  init.pl:371: 
(7)$[cl] in1t:lisp.   %  init.pl:56: 
(8)$[cl] in1t:lisp_goal.   %  init.pl:62: 
(9)$[cl] in1t:lisp_goal_pt2.   %  init.pl:67: 
(10)$[cl] cl:repl.   %  repl.pl:25: 
(11)$[cl] s7r33M:in_md(cl,(lisp_banner,set_prolog_flag(lisp_autointern,false),with_prompt_str('> ',(repeat,catch(read_eval_print(_375681506),'$aborted',fail),quietly(_375681506==end_of_file))))).   %  debugio.pl:118: 
(12)$[cl] system:(lisp_banner,set_prolog_flag(lisp_autointern,false),with_prompt_str('> ',(repeat,catch(read_eval_print(_375683772),'$aborted',fail),quietly(_375683772==end_of_file)))).  no(clause) 
(13)$[cl] cl:with_prompt_str('> ',cl:(repeat,catch(read_eval_print(_375686040),'$aborted',fail),quietly(_375686040==end_of_file))).   %  repl.pl:78: 
(14)$[system] system:ccu(cl:(repeat,catch(read_eval_print(_375688298),'$aborted',fail),quietly(_375688298==end_of_file)),cl:(prompt(_375688324,'|: '),prompts('> ',_375688332))).   %  init.pl:450: 
(15)$[system] system:scccu(system:true,cl:(repeat,catch(read_eval_print(_375690554),'$aborted',fail),quietly(_375690554==end_of_file)),_375690516,cl:(prompt(_375690580,'|: '),prompts('> ',_375690588))).   %  init.pl:443: 
(16)$[cl] system:cl:(repeat,catch(read_eval_print(_375692762),'$aborted',fail),quietly(_375692762==end_of_file)).  no(clause) 
(17)$[system] system:catch(cl:read_eval_print(_375694982),'$aborted',cl:fail).   %  init.pl:371: 
(18)$[cl] cl:read_eval_print(_375697178).   %  repl.pl:93: 
(19)*$[cl] cl:show_uncaught_or_fail(cl:eval_at_repl('dd1.',_375699388)).   %  repl.pl:74: 
(20)$[system] system:catch(cl:eval_at_repl('dd1.',_375701578),_375701564,cl:quietly((wdmsg(uncaught(_375701564)),rtrace(cl:eval_at_repl('dd1.',_375701578)),!,fail))).   %  init.pl:371: 
(21)*$[cl] cl:eval_at_repl('dd1.',_375703738).   %  repl.pl:133: 
(22)*$[cl] cl:eval_repl_hooks('dd1.',_375705900).   %  repl.pl:194: 
(23)*$[cl] prims:t_or_nil((user:dd1*->userout(yes(dd1));userout(no(dd1)),fail),_375708050).   %  sequences.pl:200: 
(24)*$[cl] system:(user:dd1*->userout(yes(dd1));userout(no(dd1)),fail).  no(clause) 
(25)$[system] system:user:dd1.   %  init.pl:308: 
(26)$[user] adfile:dd1.   %  fileload.pl:31: 
(27)$[user] adfile:cl_load('dd_compile.cl',t).   %  fileload.pl:263: 
(28)$[user] adfile:cl_load('dd_compile.cl',[],t).   %  fileload.pl:271: 
(29)$[user] adfile:with_each_file(load_1file([]),'dd_compile.cl').   %  fileload.pl:323: 
(30)$[user] adfile:with1file(load_1file([]),'dd_compile.cl').   %  fileload.pl:306: 
(31)$[user] '8ball':always(call(load_1file([]),'dd_compile.cl')).   %  eightball.pl:72: 
(32)*$[user] '8ball':nonquietly_must_or_rtrace(user:call(load_1file([]),'dd_compile.cl')).   %  eightball.pl:110: 
(33)$[system] system:catch(user:call(load_1file([]),'dd_compile.cl'),_375728890,user:gripe_problem(uncaught(_375728890),(rtrace(user:call(load_1file([]),'dd_compile.cl')),!,fail))).   %  init.pl:371: 
(34)$[system] system:call(user:load_1file([]),'dd_compile.cl').   %  init.pl:310: 
(35)$[user] adfile:load_1file([],'dd_compile.cl').   %  fileload.pl:274: 
(36)$[user] '8ball':always(locally_let([sym('cl:*readtable*')=value(sym('*readtable*')),sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl')))).   %  eightball.pl:72: 
(37)*$[user] '8ball':nonquietly_must_or_rtrace(user:locally_let([sym('cl:*readtable*')=value(sym('*readtable*')),sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl')))).   %  eightball.pl:110: 
(38)$[system] system:catch(user:locally_let([sym('cl:*readtable*')=value(sym('*readtable*')),sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),_375738860,user:gripe_problem(uncaught(_375738860),(rtrace(user:locally_let([sym('cl:*readtable*')=value(sym('*readtable*')),sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl')))),!,fail))).   %  init.pl:371: 
(39)$[callp] callp:locally_let([sym('cl:*readtable*')=value(sym('*readtable*')),sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:136: 
(40)$[callp] callp:locally_let([sym('cl:*readtable*')=read_table_znst_3,sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:137: 
(41)$[callp] callp:locally_let([xx_readtable_xx=read_table_znst_3,sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:139: 
(42)$[system] system:sccu(callp:set_opv(xx_readtable_xx,value,read_table_znst_3),callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_readtable_xx,value,read_table_znst_3)),callp:set_opv(xx_readtable_xx,value,read_table_znst_3)).   %  init.pl:447: 
(43)$[system] system:scccu(callp:set_opv(xx_readtable_xx,value,read_table_znst_3),callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_readtable_xx,value,read_table_znst_3)),_375748532,callp:set_opv(xx_readtable_xx,value,read_table_znst_3)).   %  init.pl:443: 
(44)$[callp] system:callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_readtable_xx,value,read_table_znst_3)).  no(clause) 
(45)$[callp] callp:locally_let([sym('cl:*package*')=value(sym('*package*')),sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:136: 
(46)$[callp] callp:locally_let([sym('cl:*package*')=pkg_user,sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:137: 
(47)$[callp] callp:locally_let([xx_package_xx=pkg_user,sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:139: 
(48)$[system] system:sccu(callp:set_opv(xx_package_xx,value,pkg_user),callp:(locally_let([sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_package_xx,value,pkg_user)),callp:set_opv(xx_package_xx,value,pkg_user)).   %  init.pl:447: 
(49)$[system] system:scccu(callp:set_opv(xx_package_xx,value,pkg_user),callp:(locally_let([sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_package_xx,value,pkg_user)),_375759740,callp:set_opv(xx_package_xx,value,pkg_user)).   %  init.pl:443: 
(50)$[callp] system:callp:(locally_let([sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(xx_package_xx,value,pkg_user)).  no(clause) 
(51)$[callp] callp:locally_let([sym('sys::*compiler-mode*')=sym(':load-toplevel')],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:136: 
(52)$[callp] callp:locally_let([sym('sys::*compiler-mode*')=kw_load_toplevel],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:137: 
(53)$[callp] callp:locally_let([sys_xx_compiler_mode_xx=kw_load_toplevel],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:139: 
(54)$[system] system:sccu(callp:set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel),callp:(locally_let([],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(sys_xx_compiler_mode_xx,value,kw_execute)),callp:set_opv(sys_xx_compiler_mode_xx,value,kw_execute)).   %  init.pl:447: 
(55)$[system] system:scccu(callp:set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel),callp:(locally_let([],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(sys_xx_compiler_mode_xx,value,kw_execute)),_375770516,callp:set_opv(sys_xx_compiler_mode_xx,value,kw_execute)).   %  init.pl:443: 
(56)$[callp] system:callp:(locally_let([],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),set_opv(sys_xx_compiler_mode_xx,value,kw_execute)).  no(clause) 
(57)$[callp] callp:locally_let([],locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:146: 
(58)$[callp] callp:call_interned_eval(locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  callp.pl:120: 
(59)$[callp] '8ball':always(locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  eightball.pl:72: 
(60)*$[callp] '8ball':nonquietly_must_or_rtrace(user:locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))).   %  eightball.pl:110: 
(61)$[system] system:catch(user:locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl')),_375780858,callp:gripe_problem(uncaught(_375780858),(rtrace(user:locally(t_l:sreader_options(with_text,true),with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'))),!,fail))).   %  init.pl:371: 
(62)$[_local] '_local':locally(t_l:sreader_options(with_text,true),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl')).   %  with_thread_local.pl:128: 
(63)$[_local] '_local':wtl(t_l,sreader_options(with_text,true),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),user:eccu).   %  with_thread_local.pl:166: 
(64)$[_local] '_local':wtl(t_l,t_l:sreader_options(with_text,true),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),user:eccu).   %  with_thread_local.pl:171: 
(65)$[_local] '_local':wtl_how(user:eccu,'_local':clause_true(t_l,t_l:sreader_options(with_text,true)),'_local':key_asserta(t_l,t_l:sreader_options(with_text,true)),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),'_local':key_erase(t_l)).   %  with_thread_local.pl:184: 
(66)$[leanup] leanup:eccu('_local':key_asserta(t_l,t_l:sreader_options(with_text,true)),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),'_local':key_erase(t_l)).   %  each_call_cleanup.pl:113: 
(67)$[leanup] leanup:trusted_redo_call_cleanup('_local':key_asserta(t_l,t_l:sreader_options(with_text,true)),user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),'_local':key_erase(t_l)).   %  each_call_cleanup.pl:84: 
(68)$[system] system:catch(leanup:((user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),deterministic(_375792416)),'$sig_atomic'('_local':key_erase(t_l)),(_375792416==true->!;true;'$sig_atomic'('_local':key_asserta(t_l,t_l:sreader_options(with_text,true))),fail)),_375792380,leanup:('$sig_atomic'('_local':key_erase(t_l)),throw(_375792380))).   %  init.pl:371: 
(69)$[leanup] system:leanup:((user:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl'),deterministic(_375793996)),'$sig_atomic'('_local':key_erase(t_l)),(_375793996==true->!;true;'$sig_atomic'('_local':key_asserta(t_l,t_l:sreader_options(with_text,true))),fail)).  no(clause) 
(70)$[user] adfile:with_each_form(lisp_reader_compiled_eval,'dd_compile.cl').   %  fileload.pl:309: 
(71)$[sxpr] sxpr:with_lisp_translation(file('dd_compile.cl'),user:lisp_reader_compiled_eval).   %  sreader.pl:67: 
(72)$[system] system:sccu(sxpr:l_open_input(file('dd_compile.cl'),<stream>(0x4e012c0)),sxpr:with_lisp_translation(<stream>(0x4e012c0),user:lisp_reader_compiled_eval),sxpr:ignore(notrace_catch_fail(close(<stream>(0x4e012c0)),_375798752,true))).   %  init.pl:447: 
(73)$[system] system:scccu(sxpr:l_open_input(file('dd_compile.cl'),<stream>(0x4e012c0)),sxpr:with_lisp_translation(<stream>(0x4e012c0),user:lisp_reader_compiled_eval),_375800252,sxpr:ignore(notrace_catch_fail(close(<stream>(0x4e012c0)),_375800304,true))).   %  init.pl:443: 
(74)$[sxpr] sxpr:with_lisp_translation(<stream>(0x4e012c0),user:lisp_reader_compiled_eval).   %  sreader.pl:61: 
(75)$[_trace] '_trace':must_det(sxpr:once(call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")))).   %  must_trace.pl:139: 
(76)$[_trace] '_trace':must_once(sxpr:once(call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")))).   %  must_trace.pl:130: 
(77)$[_trace] '_trace':must(sxpr:once(call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")))).   %  must_trace.pl:56: 
(78)*$[sxpr] system:sxpr:(once(call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")))*->true;throw(failed_must(once(call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")))))).  no(clause) 
(79)$[system] system:once(sxpr:call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n"))).   %  init.pl:344: 
(80)$[sxpr] sxpr:call_proc(user:lisp_reader_compiled_eval,with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")).   %  sreader.pl:72: 
(81)$[user] adfile:lisp_reader_compiled_eval(with_text(['compile-file','$STRING'("dd_gen")],"(compile-file \"dd_gen\")\n")).   %  fileload.pl:284: 
(82)$[user] adfile:lisp_reader_compiled_eval(['compile-file','$STRING'("dd_gen")]).   %  fileload.pl:285: 
(83)$[user] '8ball':always((as_sexp(['compile-file','$STRING'("dd_gen")],['compile-file','$STRING'("dd_gen")]),both_outputs(write_file_info),reading_package(pkg_user),dbginfo(flat((:-lisp_compile_to_prolog(pkg_user,['compile-file','$STRING'("dd_gen")])))),reader_intern_symbols(['compile-file','$STRING'("dd_gen")],[compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]))).   %  eightball.pl:63: 
(84)$[user] '8ball':always((both_outputs(write_file_info),reading_package(pkg_user),dbginfo(flat((:-lisp_compile_to_prolog(pkg_user,['compile-file','$STRING'("dd_gen")])))),reader_intern_symbols(['compile-file','$STRING'("dd_gen")],[compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]))).   %  eightball.pl:63: 
(85)$[user] '8ball':always((reading_package(pkg_user),dbginfo(flat((:-lisp_compile_to_prolog(pkg_user,['compile-file','$STRING'("dd_gen")])))),reader_intern_symbols(['compile-file','$STRING'("dd_gen")],[compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]))).   %  eightball.pl:63: 
(86)$[user] '8ball':always((dbginfo(flat((:-lisp_compile_to_prolog(pkg_user,['compile-file','$STRING'("dd_gen")])))),reader_intern_symbols(['compile-file','$STRING'("dd_gen")],[compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]))).   %  eightball.pl:63: 
(87)$[user] '8ball':always((reader_intern_symbols(['compile-file','$STRING'("dd_gen")],[compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]))).   %  eightball.pl:63: 
(88)$[user] '8ball':always(process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])])).   %  eightball.pl:72: 
(89)*$[user] '8ball':nonquietly_must_or_rtrace(user:process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])])).   %  eightball.pl:110: 
(90)$[system] system:catch(user:process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]),_375824764,user:gripe_problem(uncaught(_375824764),(rtrace(user:process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])])),!,fail))).   %  init.pl:371: 
(91)$[user] adfile:process_load_expression([compile_file,'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])]).   %  fileload.pl:296: 
(92)$[user] system:with_output_to(user_error,sccu(format('~N/*~n',[]),always(cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375827432)),format('~N*/~n',[]))).  no(clause) 
(93)$[system] system:'$c_call_prolog'.  no(clause) 
(94)$[system] system:sccu(user:format('~N/*~n',[]),user:always(cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375830060)),user:format('~N*/~n',[])).   %  init.pl:447: 
(95)$[system] system:scccu(user:format('~N/*~n',[]),user:always(cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375831348)),_375831316,user:format('~N*/~n',[])).   %  init.pl:443: 
(96)$[user] '8ball':always(cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375832594)).   %  eightball.pl:72: 
(97)*$[user] '8ball':nonquietly_must_or_rtrace(user:cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375833862)).   %  eightball.pl:110: 
(98)$[system] system:catch(user:cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375835116),_375835100,user:gripe_problem(uncaught(_375835100),(rtrace(user:cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375835116)),!,fail))).   %  init.pl:371: 
(99)$[user] adfile:cl_compile_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),[],_375836340).   %  fileload.pl:145: 
(100)$[user] adfile:do_compile_1file([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)])).   %  fileload.pl:152: 
(101)$[callp] callp:locally_let([sym('sys::*compile-file-pathname*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(102)$[callp] callp:locally_let([sym('sys::*compile-file-pathname*')='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)])),sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(103)$[callp] callp:locally_let([xx_compile_file_pathname_xx='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)])),sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(104)$[system] system:sccu(callp:set_opv(xx_compile_file_pathname_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)]))),callp:(locally_let([sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_pathname_xx,value,[])),callp:set_opv(xx_compile_file_pathname_xx,value,[])).   %  init.pl:447: 
(105)$[system] system:scccu(callp:set_opv(xx_compile_file_pathname_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)]))),callp:(locally_let([sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_pathname_xx,value,[])),_375843516,callp:set_opv(xx_compile_file_pathname_xx,value,[])).   %  init.pl:443: 
(106)$[callp] system:callp:(locally_let([sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_pathname_xx,value,[])).  no(clause) 
(107)$[callp] callp:locally_let([sym('sys::*compile-file-truename*')=path(str('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(108)$[callp] callp:locally_let([sym('sys::*compile-file-truename*')='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)])),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(109)$[callp] callp:locally_let([xx_compile_file_truename_xx='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)])),sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(110)$[system] system:sccu(callp:set_opv(xx_compile_file_truename_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)]))),callp:(locally_let([sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_truename_xx,value,[])),callp:set_opv(xx_compile_file_truename_xx,value,[])).   %  init.pl:447: 
(111)$[system] system:scccu(callp:set_opv(xx_compile_file_truename_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l)]))),callp:(locally_let([sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_truename_xx,value,[])),_375850260,callp:set_opv(xx_compile_file_truename_xx,value,[])).   %  init.pl:443: 
(112)$[callp] system:callp:(locally_let([sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_compile_file_truename_xx,value,[])).  no(clause) 
(113)$[callp] callp:locally_let([sym('sys::*compiler-mode*')=sym(':compile-toplevel'),sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(114)$[callp] callp:locally_let([sym('sys::*compiler-mode*')=kw_compile_toplevel,sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(115)$[callp] callp:locally_let([sys_xx_compiler_mode_xx=kw_compile_toplevel,sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(116)$[system] system:sccu(callp:set_opv(sys_xx_compiler_mode_xx,value,kw_compile_toplevel),callp:(locally_let([sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel)),callp:set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel)).   %  init.pl:447: 
(117)$[system] system:scccu(callp:set_opv(sys_xx_compiler_mode_xx,value,kw_compile_toplevel),callp:(locally_let([sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel)),_375856572,callp:set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel)).   %  init.pl:443: 
(118)$[callp] system:callp:(locally_let([sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_compiler_mode_xx,value,kw_load_toplevel)).  no(clause) 
(119)$[callp] callp:locally_let([sym('sys::*output-file-pathname*')=path('$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(120)$[callp] callp:locally_let([sym('sys::*output-file-pathname*')='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)])),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(121)$[callp] callp:locally_let([sys_xx_output_file_pathname_xx='$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)])),sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(122)$[system] system:sccu(callp:set_opv(sys_xx_output_file_pathname_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_output_file_pathname_xx,value,[])),callp:set_opv(sys_xx_output_file_pathname_xx,value,[])).   %  init.pl:447: 
(123)$[system] system:scccu(callp:set_opv(sys_xx_output_file_pathname_xx,value,'$OBJ'(claz_pathname,'$ARRAY'([*],claz_base_character,[#\(/),#\(h),#\(o),#\(m),#\(e),#\(/),#\(d),#\(m),#\(i),#\(l),#\(e),#\(s),#\(/),#\(l),#\(o),#\(g),#\(i),#\(c),#\(m),#\(o),#\(o),#\('_'),#\(w),#\(o),#\(r),#\(k),#\(s),#\(p),#\(a),#\(c),#\(e),#\(/),#\(p),#\(a),#\(c),#\(k),#\(s),#\('_'),#\(u),#\(s),#\(r),#\(/),#\(w),#\(a),#\(m),#\('_'),#\(c),#\(o),#\(m),#\(m),#\(o),#\(n),#\('_'),#\(l),#\(i),#\(s),#\(p),#\(/),#\(t),#\(/),#\(d),#\(a),#\(y),#\(d),#\(r),#\(e),#\(a),#\(m),#\(e),#\(r),#\(/),#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n),#\('.'),#\(c),#\(l),#\('.'),#\(p),#\(r),#\(o)]))),callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_output_file_pathname_xx,value,[])),_375862452,callp:set_opv(sys_xx_output_file_pathname_xx,value,[])).   %  init.pl:443: 
(124)$[callp] system:callp:(locally_let([sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(sys_xx_output_file_pathname_xx,value,[])).  no(clause) 
(125)$[callp] callp:locally_let([sym('cl:*package*')=value(sym('*package*')),sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(126)$[callp] callp:locally_let([sym('cl:*package*')=pkg_user,sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(127)$[callp] callp:locally_let([xx_package_xx=pkg_user,sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(128)$[system] system:sccu(callp:set_opv(xx_package_xx,value,pkg_user),callp:(locally_let([sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_package_xx,value,pkg_user)),callp:set_opv(xx_package_xx,value,pkg_user)).   %  init.pl:447: 
(129)$[system] system:scccu(callp:set_opv(xx_package_xx,value,pkg_user),callp:(locally_let([sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_package_xx,value,pkg_user)),_375867900,callp:set_opv(xx_package_xx,value,pkg_user)).   %  init.pl:443: 
(130)$[callp] system:callp:(locally_let([sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_package_xx,value,pkg_user)).  no(clause) 
(131)$[callp] callp:locally_let([sym('cl:*readtable*')=value(sym('*readtable*'))],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:136: 
(132)$[callp] callp:locally_let([sym('cl:*readtable*')=read_table_znst_3],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:137: 
(133)$[callp] callp:locally_let([xx_readtable_xx=read_table_znst_3],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:139: 
(134)$[system] system:sccu(callp:set_opv(xx_readtable_xx,value,read_table_znst_3),callp:(locally_let([],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_readtable_xx,value,read_table_znst_3)),callp:set_opv(xx_readtable_xx,value,read_table_znst_3)).   %  init.pl:447: 
(135)$[system] system:scccu(callp:set_opv(xx_readtable_xx,value,read_table_znst_3),callp:(locally_let([],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_readtable_xx,value,read_table_znst_3)),_375872916,callp:set_opv(xx_readtable_xx,value,read_table_znst_3)).   %  init.pl:443: 
(136)$[callp] system:callp:(locally_let([],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),set_opv(xx_readtable_xx,value,read_table_znst_3)).  no(clause) 
(137)$[callp] callp:locally_let([],sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:146: 
(138)$[callp] callp:call_interned_eval(sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  callp.pl:120: 
(139)$[callp] '8ball':always(sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  eightball.pl:72: 
(140)*$[callp] '8ball':nonquietly_must_or_rtrace(user:sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))).   %  eightball.pl:110: 
(141)$[system] system:catch(user:sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20))),_375877498,callp:gripe_problem(uncaught(_375877498),(rtrace(user:sccu(open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),close(<stream>(0x5130e20)))),!,fail))).   %  init.pl:371: 
(142)$[system] system:sccu(user:open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),user:do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),user:close(<stream>(0x5130e20))).   %  init.pl:447: 
(143)$[system] system:scccu(user:open('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl.pro',write,<stream>(0x5130e20)),user:do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)),_375878932,user:close(<stream>(0x5130e20))).   %  init.pl:443: 
(144)$[user] adfile:do_compile_1file_to_stream([],'$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),<stream>(0x5130e20)).   %  fileload.pl:171: 
(145)$[user] '8ball':always((pl_probe_file('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),to_prolog_string_anyways('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),"dd_gen"),get_time(1514093033.1488676),format_time(string("Sat Dec 23 21:23:53 2017"),'%+',1514093033.1488676),working_directory('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/'),statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375880626,_375880632]),_375880644 is (_375880626-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375880644]))).   %  eightball.pl:63: 
(146)$[user] '8ball':always((to_prolog_string_anyways('$ARRAY'([*],claz_base_character,[#\(d),#\(d),#\('_'),#\(g),#\(e),#\(n)]),"dd_gen"),get_time(1514093033.1488676),format_time(string("Sat Dec 23 21:23:53 2017"),'%+',1514093033.1488676),working_directory('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/'),statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375881288,_375881294]),_375881306 is (_375881288-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375881306]))).   %  eightball.pl:63: 
(147)$[user] '8ball':always((get_time(1514093033.1488676),format_time(string("Sat Dec 23 21:23:53 2017"),'%+',1514093033.1488676),working_directory('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/'),statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375881858,_375881864]),_375881876 is (_375881858-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375881876]))).   %  eightball.pl:63: 
(148)$[user] '8ball':always((format_time(string("Sat Dec 23 21:23:53 2017"),'%+',1514093033.1488676),working_directory('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/'),statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375882492,_375882498]),_375882510 is (_375882492-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375882510]))).   %  eightball.pl:63: 
(149)$[user] '8ball':always((working_directory('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/'),statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375883094,_375883100]),_375883112 is (_375883094-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375883112]))).   %  eightball.pl:63: 
(150)$[user] '8ball':always((statistics(runtime,[89782,32]),format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375883708,_375883714]),_375883726 is (_375883708-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375883726]))).   %  eightball.pl:63: 
(151)$[user] '8ball':always((format(<stream>(0x5130e20),'#!/usr/bin/env swipl\n%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )\n%; File: ~q (~w)\n%; PWD: ~w\n%; Start time: ~w\n\n:-style_check(-discontiguous).\n:-style_check(-singleton).\n:-use_module(library(wamcl_runtime)).\n\n',["dd_gen",'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl','/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/',"Sat Dec 23 21:23:53 2017"]),with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375884298,_375884304]),_375884316 is (_375884298-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375884316]))).   %  eightball.pl:63: 
(152)$[user] '8ball':always((with_output_to(<stream>(0x5130e20),statistics),locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375884844,_375884850]),_375884862 is (_375884844-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375884862]))).   %  eightball.pl:63: 
(153)$[user] '8ball':always((locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),statistics(runtime,[_375885422,_375885428]),_375885440 is (_375885422-89782)/1000,format(<stream>(0x5130e20),'~n~n%; Total compilation time: ~w seconds~n~n',[_375885440]))).   %  eightball.pl:63: 
(154)$[user] '8ball':always(locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'))).   %  eightball.pl:72: 
(155)*$[user] '8ball':nonquietly_must_or_rtrace(user:locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'))).   %  eightball.pl:110: 
(156)$[system] system:catch(user:locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),_375887068,user:gripe_problem(uncaught(_375887068),(rtrace(user:locally(t_l:sreader_options(with_text,true),with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'))),!,fail))).   %  init.pl:371: 
(157)$[_local] '_local':locally(t_l:sreader_options(with_text,true),user:with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')).   %  with_thread_local.pl:128: 
(158)$[_local] '_local':wtl(t_l,sreader_options(with_text,true),user:with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),user:eccu).   %  with_thread_local.pl:166: 
(159)$[_local] '_local':wtl(t_l,t_l:sreader_options(with_text,true),user:with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),user:eccu).   %  with_thread_local.pl:171: 
(160)$[_local] '_local':wtl_how(user:eccu,'_local':clause_true(t_l,t_l:sreader_options(with_text,true)),'_local':key_asserta(t_l,t_l:sreader_options(with_text,true)),user:with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),'_local':key_erase(t_l)).   %  with_thread_local.pl:184: 
(161)$[user] adfile:with_each_file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl').   %  fileload.pl:323: 
(162)$[user] adfile:with1file(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl').   %  fileload.pl:306: 
(163)$[user] '8ball':always(call(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')).   %  eightball.pl:72: 
(164)*$[user] '8ball':nonquietly_must_or_rtrace(user:call(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')).   %  eightball.pl:110: 
(165)$[system] system:catch(user:call(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),_375891514,user:gripe_problem(uncaught(_375891514),(rtrace(user:call(with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl')),!,fail))).   %  init.pl:371: 
(166)$[system] system:call(user:with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20))),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl').   %  init.pl:310: 
(167)$[user] adfile:with_each_form(lisp_compile_to_prolog_output(<stream>(0x5130e20)),'/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl').   %  fileload.pl:309: 
(168)$[sxpr] sxpr:with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),user:lisp_compile_to_prolog_output(<stream>(0x5130e20))).   %  sreader.pl:67: 
(169)$[system] system:sccu(sxpr:l_open_input(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),<stream>(0x512d630)),sxpr:with_lisp_translation(<stream>(0x512d630),user:lisp_compile_to_prolog_output(<stream>(0x5130e20))),sxpr:ignore(notrace_catch_fail(close(<stream>(0x512d630)),_375893234,true))).   %  init.pl:447: 
(170)$[system] system:scccu(sxpr:l_open_input(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl'),<stream>(0x512d630)),sxpr:with_lisp_translation(<stream>(0x512d630),user:lisp_compile_to_prolog_output(<stream>(0x5130e20))),_375893566,sxpr:ignore(notrace_catch_fail(close(<stream>(0x512d630)),_375893622,true))).   %  init.pl:443: 
(171)$[sxpr] sxpr:with_lisp_translation(<stream>(0x512d630),user:lisp_compile_to_prolog_output(<stream>(0x5130e20))).   %  sreader.pl:61: 
(172)$[_trace] '_trace':must_det(sxpr:once(call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")))).   %  must_trace.pl:139: 
(173)$[_trace] '_trace':must_once(sxpr:once(call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")))).   %  must_trace.pl:130: 
(174)$[_trace] '_trace':must(sxpr:once(call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")))).   %  must_trace.pl:56: 
(175)*$[sxpr] system:sxpr:(once(call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")))*->true;throw(failed_must(once(call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")))))).  no(clause) 
(176)$[system] system:once(sxpr:call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n"))).   %  init.pl:344: 
(177)$[sxpr] sxpr:call_proc(user:lisp_compile_to_prolog_output(<stream>(0x5130e20)),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")).   %  sreader.pl:72: 
(178)$[user] adfile:lisp_compile_to_prolog_output(<stream>(0x5130e20),with_text(['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']],"(define-gen EPISODE nil\n  (let ((goal (ob$get con 'goal)))\n     (gen-subgoals goal stream switches context bp t)\n     *me-ob*))\n\n")).   %  fileload.pl:197: 
(179)$[user] adfile:lisp_compile_to_prolog_output(<stream>(0x5130e20),['define-gen','EPISODE',nil,[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']]).   %  fileload.pl:198: 
(180)$[user] system:with_output_to(<stream>(0x5130e20),lisp_compile_to_prolog(['define-gen','EPISODE',[],[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']])).  no(clause) 
(181)$[system] system:'$c_call_prolog'.  no(clause) 
(182)$[user] adfile:lisp_compile_to_prolog(['define-gen','EPISODE',[],[let,[[goal,['ob$get',con,[quote,goal]]]],['gen-subgoals',goal,stream,switches,context,bp,t],'*me-ob*']]).   %  fileload.pl:218: 
(183)$[user] adfile:lisp_grovel(f_u_define_gen(u_episode,[],[[let,[[u_goal,[u_ob_c36_get,u_con,[quote,u_goal]]]],[u_gen_subgoals,u_goal,stream,u_switches,u_context,u_bp,t],u_xx_me_ob_xx]],_375897498)).   %  fileload.pl:246: 
(184)$[user] f_u_define_gen(u_episode,[],[[let,[[u_goal,[u_ob_c36_get,u_con,[quote,u_goal]]]],[u_gen_subgoals,u_goal,stream,u_switches,u_context,u_bp,t],u_xx_me_ob_xx]],_375897694).  /*DYN*/ 
(185)$[user] uncall:cl_eval([let,[[u_ty,[u_ob_c36_name_c62_ob,[quote,u_episode]]]],[if,[u_null_c63,u_ty],[format,t,'$ARRAY'([*],claz_base_character,"define-gen: unknown type: ~A~%"),[quote,u_episode]],[u_ob_c36_set,u_ty,[quote,u_gen],[lambda,[u_con,stream,u_switches,u_context,u_bp],[let,[[u_goal,[u_ob_c36_get,u_con,[quote,u_goal]]]],[u_gen_subgoals,u_goal,stream,u_switches,u_context,u_bp,t],u_xx_me_ob_xx]]]]],_375897914).   %  funcall.pl:144: 
(186)$[user] '8ball':always((f_u_ob_c36_name_c62_ob(u_episode,_375898118),_375898128=[bv(u_ty,_375898118)|_375898136],f_u_null_c63(u_ty,_375898158),(_375898158\==[]->cl_format([t,'$ARRAY'([*],claz_base_character,"define-gen: unknown type: ~A~%"),u_episode],_375898198),_375898246=_375898198;get_var(_375898128,u_ty,_375898262),_375898272=closure([_375898288|_375898128],_375898294,[u_con,stream,u_switches,u_context,u_bp],(get_var(_375898288,u_con,_375898348),f_u_ob_c36_get(_375898348,u_goal,_375898362),_375898378=[bv(u_goal,_375898362)|_375898288],get_var(_375898378,stream,_375898414),get_var(_375898378,u_bp,_375898428),get_var(_375898378,u_context,_375898442),get_var(_375898378,u_goal,_375898456),get_var(_375898378,u_switches,_375898470),f_u_gen_subgoals(_375898456,_375898414,_375898470,_375898442,_375898428,t,_375898492),get_var(_375898378,u_xx_me_ob_xx,_375898294))),f_u_ob_c36_set(_375898262,u_gen,_375898272,_375898516),_375898246=_375898516))).   %  eightball.pl:63: 
(187)$[user] '8ball':always(f_u_ob_c36_name_c62_ob(u_episode,_375898294)).   %  eightball.pl:72: 
(188)*$[user] '8ball':nonquietly_must_or_rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375898470)).   %  eightball.pl:110: 
(189)$[system]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)

existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:catch(user:f_u_ob_c36_name_c62_ob(u_episode,_375898632),error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375898656)),user:gripe_problem(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375898656))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375898632)),!,fail))).   %  init.pl:371: 
(190)$[user]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 '8ball':gripe_problem(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375898792))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375898826)),!,fail)).   %  eightball.pl:115: 
(191)$[user]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 '8ball':always_catch(gripe_problem0(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375898930))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375898964)),!,fail))).   %  eightball.pl:99: 
(192)$[system]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:catch(user:catch(gripe_problem0(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899070))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899104)),!,fail)),'$aborted',notrace),_375899020,user:(dbginfo(always_uncaught(_375899020)),notrace,!,fail)).   %  init.pl:371: 
(193)$[system]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:catch(user:gripe_problem0(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899172))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899206)),!,fail)),'$aborted',user:notrace).   %  init.pl:371: 
(194)$[user]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 '8ball':gripe_problem0(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899256))),(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899290)),!,fail)).   %  eightball.pl:116: 
(195)$[user]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)

existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:notrace((dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899342)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899376)),!,fail)),dumpST,dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899342)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899376)),!,fail)))).  no(clause) 
(196)$[system] system:'$c_call_prolog'.  no(clause) 
(197)$[system]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)

existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:(user:dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899494)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899528)),!,fail)),user:(dumpST,dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899494)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899528)),!,fail)))).   %  init.pl:211: 
(198)$[system]
existence_error(procedure,f_u_ob_c36_name_c62_ob/2)

existence_error(procedure,f_u_ob_c36_name_c62_ob/2)
 system:(user:dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899534)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899568)),!,fail)),user:(dumpST,dbginfo(uncaught(error('existence_error_XXXXXXXXX__\033\[0m\033\[1;34m%-6s\033\[m\'This is text\033\[0mRED__existence_error_existence_error'(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375899534)))=(rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375899568)),!,fail)))).  no(clause) 
(199)$[user] dumpst:dumpST.   %  dumpst.pl:128: 
(200)$[user] first:zotrace((prolog_current_frame(4755),b_setval('$dump_frame',4755),dumpST1)).   %  first.pl:432: 
(201)$[user] system:(prolog_current_frame(4755),b_setval('$dump_frame',4755),dumpST1).  no(clause) 
/*
% uncaught(error(existence_error(procedure,f_u_ob_c36_name_c62_ob/2),context(system:catch/3,_375351864))):-rtrace(user:f_u_ob_c36_name_c62_ob(u_episode,_375057678)),!,fail.
*/

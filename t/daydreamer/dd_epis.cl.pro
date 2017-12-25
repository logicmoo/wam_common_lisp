#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "dd_epis" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
%; Start time: Sat Dec 23 21:23:17 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
; #- :abcl (compile-file "dd_gen")
*/
/*
; #+ :wamcl 
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
  7/22/86: Added object episodic memory
*/
/*
   8/5/86: Wrote new storage/retrieval functions
*/
/*
  9/25/86: Took out flavors
*/
/*
 10/12/86: Added episode descendants
*/
/*
*/
/*
*******************************************************************************
*/
/*
*/
/*
 Episode creation
*/
/*
*/
/*
(ty$create 'EPISODE nil '(nil (rule goal context realism desirability
                                    ordering) (ordering)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:472 **********************/
:-lisp_compile_to_prolog(pkg_user,['ty$create',[quote,'EPISODE'],[],[quote,[[],[rule,goal,context,realism,desirability,ordering],[ordering]]]])
:- f_u_ty_c36_create(u_episode,
		     [],
		     
		     [ [],
		       
		       [ u_rule,
			 u_goal,
			 u_context,
			 u_realism,
			 u_desirability,
			 u_ordering
		       ],
		       [u_ordering]
		     ],
		     _Ignored).
/*
(setq *infinite-thresh* 100)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:603 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*infinite-thresh*',100])
:- set_var(AEnv, setq, u_xx_infinite_thresh_xx, 100).
/*
(defun make-and-store-episode (rule goal context realism desirability
                                hidden? children)
  (ndbg-roman-nl *gate-dbg* ep-store "Make episode for goal "(defun make-and-store-episode (rule goal context realism desirability\n                                hidden? children)\n  (ndbg-roman-nl *gate-dbg* ep-store \"Make episode for goal ~A\" goal)\n  (let ((ep (make-episode rule goal context realism desirability)))\n       (if children\n           (progn\n            (ob$set ep 'children children)\n            (ob$set ep 'descendants  ; includes self\n                    (yloop\n                     (initial (result (list ep)))\n                     (yfor child in children)\n                     (ydo\n                      (ob$set child 'parent ep)\n                      (setq result (append! result\n                                            (copy-list\n                                             (ob$get child 'descendants)))))\n                     (yresult result))))\n           (ob$set ep 'descendants (list ep)))\n       (if hidden?\n           (progn\n            (ob$set ep 'plan-threshold *infinite-thresh*)\n            (ob$set ep 'reminding-threshold *infinite-thresh*)\n            (epmem-store ep rule nil nil))\n           (progn\n            (ob$set rule 'accessible? t)\n            (epmem-store ep rule t t)))\n       ep))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:633 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'make-and-store-episode',[rule,goal,context,realism,desirability,'hidden?',children],['ndbg-roman-nl','*gate-dbg*','ep-store','$STRING'("Make episode for goal ~A"),goal],[let,[[ep,['make-episode',rule,goal,context,realism,desirability]]],[if,children,[progn,['ob$set',ep,[quote,children],children],['ob$set',ep,[quote,descendants],[yloop,[initial,[result,[list,ep]]],[yfor,child,in,children],[ydo,['ob$set',child,[quote,parent],ep],[setq,result,['append!',result,['copy-list',['ob$get',child,[quote,descendants]]]]]],[yresult,result]]]],['ob$set',ep,[quote,descendants],[list,ep]]],[if,'hidden?',[progn,['ob$set',ep,[quote,'plan-threshold'],'*infinite-thresh*'],['ob$set',ep,[quote,'reminding-threshold'],'*infinite-thresh*'],['epmem-store',ep,rule,[],[]]],[progn,['ob$set',rule,[quote,'accessible?'],t],['epmem-store',ep,rule,t,t]]],ep]])
wl:lambda_def(defun, u_make_and_store_episode, f_u_make_and_store_episode, [u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Make episode for goal ~A"), u_goal], [let, [[u_ep, [u_make_episode, u_rule, u_goal, u_context, u_realism, u_desirability]]], [if, u_children, [progn, [u_ob_c36_set, u_ep, [quote, u_children], u_children], [u_ob_c36_set, u_ep, [quote, u_descendants], [u_yloop, [u_initial, [u_result, [list, u_ep]]], [u_yfor, u_child, u_in, u_children], [u_ydo, [u_ob_c36_set, u_child, [quote, u_parent], u_ep], [setq, u_result, [u_append_c33, u_result, [copy_list, [u_ob_c36_get, u_child, [quote, u_descendants]]]]]], [u_yresult, u_result]]]], [u_ob_c36_set, u_ep, [quote, u_descendants], [list, u_ep]]], [if, u_hidden_c63, [progn, [u_ob_c36_set, u_ep, [quote, u_plan_threshold], u_xx_infinite_thresh_xx], [u_ob_c36_set, u_ep, [quote, u_reminding_threshold], u_xx_infinite_thresh_xx], [u_epmem_store, u_ep, u_rule, [], []]], [progn, [u_ob_c36_set, u_rule, [quote, u_accessible_c63], t], [u_epmem_store, u_ep, u_rule, t, t]]], u_ep]]).
wl:arglist_info(u_make_and_store_episode, f_u_make_and_store_episode, [u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], arginfo{all:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], opt:0, req:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_make_and_store_episode).

/*

### Compiled:  `U::MAKE-AND-STORE-EPISODE` 
*/
f_u_make_and_store_episode(Rule, Goal, Context, Realism, Desirability, Hidden_c63, Children, FnResult) :-
	nop(global_env(Env)),
	Env40=[bv(u_rule, Rule), bv(u_goal, Goal), bv(u_context, Context), bv(u_realism, Realism), bv(u_desirability, Desirability), bv(u_hidden_c63, Hidden_c63), bv(u_children, Children)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Make episode for goal ~A"),
			    u_goal
			  ],
			  Roman_nl_Ret),
	get_var(Env40, u_context, Context_Get),
	get_var(Env40, u_desirability, Desirability_Get),
	get_var(Env40, u_goal, Goal_Get),
	get_var(Env40, u_realism, Realism_Get),
	get_var(Env40, u_rule, Rule_Get),
	f_u_make_episode(Rule_Get,
			 Goal_Get,
			 Context_Get,
			 Realism_Get,
			 Desirability_Get,
			 Ep_Init),
	LEnv=[bv(u_ep, Ep_Init)|Env40],
	get_var(LEnv, u_children, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_children, Children_Get17),
	    get_var(LEnv, u_ep, Ep_Get),
	    f_u_ob_c36_set(Ep_Get, u_children, Children_Get17, C36_set_Ret),
	    get_var(LEnv, u_ep, Ep_Get18),
	    f_u_yloop(
		      [ [u_initial, [u_result, [list, u_ep]]],
			[u_yfor, u_child, u_in, u_children],
			
			[ u_ydo,
			  [u_ob_c36_set, u_child, [quote, u_parent], u_ep],
			  
			  [ setq,
			    u_result,
			    
			    [ u_append_c33,
			      u_result,
			      
			      [ copy_list,
				[u_ob_c36_get, u_child, [quote, u_descendants]]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      Descendants),
	    f_u_ob_c36_set(Ep_Get18, u_descendants, Descendants, TrueResult),
	    _206091700=TrueResult
	;   get_var(LEnv, u_ep, Ep_Get19),
	    Descendants49=[Ep_Get19],
	    f_u_ob_c36_set(Ep_Get19, u_descendants, Descendants49, ElseResult),
	    _206091700=ElseResult
	),
	get_var(LEnv, u_hidden_c63, IFTEST23),
	(   IFTEST23\==[]
	->  get_var(LEnv, u_ep, Ep_Get26),
	    get_var(LEnv, u_xx_infinite_thresh_xx, Xx_infinite_thresh_xx_Get),
	    f_u_ob_c36_set(Ep_Get26,
			   u_plan_threshold,
			   Xx_infinite_thresh_xx_Get,
			   C36_set_Ret53),
	    get_var(LEnv, u_ep, Ep_Get28),
	    get_var(LEnv, u_xx_infinite_thresh_xx, Xx_infinite_thresh_xx_Get29),
	    f_u_ob_c36_set(Ep_Get28,
			   u_reminding_threshold,
			   Xx_infinite_thresh_xx_Get29,
			   C36_set_Ret54),
	    get_var(LEnv, u_ep, Ep_Get30),
	    get_var(LEnv, u_rule, Rule_Get31),
	    f_u_epmem_store(Ep_Get30, Rule_Get31, [], [], TrueResult35),
	    _206163898=TrueResult35
	;   get_var(LEnv, u_rule, Rule_Get32),
	    f_u_ob_c36_set(Rule_Get32, u_accessible_c63, t, T),
	    get_var(LEnv, u_ep, Ep_Get33),
	    get_var(LEnv, u_rule, Rule_Get34),
	    f_u_epmem_store(Ep_Get33, Rule_Get34, t, t, ElseResult36),
	    _206163898=ElseResult36
	),
	get_var(LEnv, u_ep, Ep_Get37),
	Ep_Get37=FnResult.
:- set_opv(f_u_make_and_store_episode, classof, claz_function),
   set_opv(u_make_and_store_episode, compile_as, kw_function),
   set_opv(u_make_and_store_episode, function, f_u_make_and_store_episode),
   DefunResult=u_make_and_store_episode.
/*
:- side_effect(assert_lsp(u_make_and_store_episode,
			  wl:lambda_def(defun, u_make_and_store_episode, f_u_make_and_store_episode, [u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Make episode for goal ~A"), u_goal], [let, [[u_ep, [u_make_episode, u_rule, u_goal, u_context, u_realism, u_desirability]]], [if, u_children, [progn, [u_ob_c36_set, u_ep, [quote, u_children], u_children], [u_ob_c36_set, u_ep, [quote, u_descendants], [u_yloop, [u_initial, [u_result, [list, u_ep]]], [u_yfor, u_child, u_in, u_children], [u_ydo, [u_ob_c36_set, u_child, [quote, u_parent], u_ep], [setq, u_result, [u_append_c33, u_result, [copy_list, [u_ob_c36_get, u_child, [quote, u_descendants]]]]]], [u_yresult, u_result]]]], [u_ob_c36_set, u_ep, [quote, u_descendants], [list, u_ep]]], [if, u_hidden_c63, [progn, [u_ob_c36_set, u_ep, [quote, u_plan_threshold], u_xx_infinite_thresh_xx], [u_ob_c36_set, u_ep, [quote, u_reminding_threshold], u_xx_infinite_thresh_xx], [u_epmem_store, u_ep, u_rule, [], []]], [progn, [u_ob_c36_set, u_rule, [quote, u_accessible_c63], t], [u_epmem_store, u_ep, u_rule, t, t]]], u_ep]]))).
*/
/*
:- side_effect(assert_lsp(u_make_and_store_episode,
			  wl:arglist_info(u_make_and_store_episode, f_u_make_and_store_episode, [u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], arginfo{all:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], opt:0, req:[u_rule, u_goal, u_context, u_realism, u_desirability, u_hidden_c63, u_children], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_make_and_store_episode,
			  wl:init_args(exact_only, f_u_make_and_store_episode))).
*/
/*
 includes self
*/
/*
(defun accessible? (rule)
  (ob$get rule 'accessible?))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1806 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'accessible?',[rule],['ob$get',rule,[quote,'accessible?']]])
wl:lambda_def(defun, u_accessible_c63, f_u_accessible_c63, [u_rule], [[u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]).
wl:arglist_info(u_accessible_c63, f_u_accessible_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_accessible_c63).

/*

### Compiled:  `U::ACCESSIBLE?` 
*/
f_u_accessible_c63(Rule, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_rule, Rule)|Env],
	get_var(Env7, u_rule, Rule_Get),
	f_u_ob_c36_get(Rule_Get, u_accessible_c63, Accessible_c63),
	Accessible_c63=FnResult.
:- set_opv(f_u_accessible_c63, classof, claz_function),
   set_opv(u_accessible_c63, compile_as, kw_function),
   set_opv(u_accessible_c63, function, f_u_accessible_c63),
   DefunResult=u_accessible_c63.
/*
:- side_effect(assert_lsp(u_accessible_c63,
			  wl:lambda_def(defun, u_accessible_c63, f_u_accessible_c63, [u_rule], [[u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]))).
*/
/*
:- side_effect(assert_lsp(u_accessible_c63,
			  wl:arglist_info(u_accessible_c63, f_u_accessible_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_accessible_c63,
			  wl:init_args(exact_only, f_u_accessible_c63))).
*/
/*
(defun inaccessible? (rule)
  (null? (ob$get rule 'accessible?)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1863 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'inaccessible?',[rule],['null?',['ob$get',rule,[quote,'accessible?']]]])
wl:lambda_def(defun, u_inaccessible_c63, f_u_inaccessible_c63, [u_rule], [[u_null_c63, [u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]]).
wl:arglist_info(u_inaccessible_c63, f_u_inaccessible_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_inaccessible_c63).

/*

### Compiled:  `U::INACCESSIBLE?` 
*/
f_u_inaccessible_c63(Rule, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_rule, Rule)|Env],
	f_u_null_c63([u_ob_c36_get, u_rule, [quote, u_accessible_c63]],
		     Null_c63_Ret),
	Null_c63_Ret=FnResult.
:- set_opv(f_u_inaccessible_c63, classof, claz_function),
   set_opv(u_inaccessible_c63, compile_as, kw_function),
   set_opv(u_inaccessible_c63, function, f_u_inaccessible_c63),
   DefunResult=u_inaccessible_c63.
/*
:- side_effect(assert_lsp(u_inaccessible_c63,
			  wl:lambda_def(defun, u_inaccessible_c63, f_u_inaccessible_c63, [u_rule], [[u_null_c63, [u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]]))).
*/
/*
:- side_effect(assert_lsp(u_inaccessible_c63,
			  wl:arglist_info(u_inaccessible_c63, f_u_inaccessible_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_inaccessible_c63,
			  wl:init_args(exact_only, f_u_inaccessible_c63))).
*/
/*
(setq *next-ep-number* 1)

; The below is for debugging purposes only.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1930 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*next-ep-number*',1])
:- set_var(AEnv, setq, u_xx_next_ep_number_xx, 1).
/*
 The below is for debugging purposes only.
*/
/*
(setq *episodes* nil)

; We want two level eps to be stored. E.g., Harrison goes to Cairo.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2001 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*episodes*',[]])
:- set_var(AEnv, setq, u_xx_episodes_xx, []).
/*
 We want two level eps to be stored. E.g., Harrison goes to Cairo.
*/
/*
(defun make-episode (rule goal context realism desirability)
  (let ((ep (ob$fcreate `(EPISODE rule ,rule goal ,goal
                                  context ,context
                                  realism ,(or realism
                                               (strength (ob$get goal 'obj)))
                                  desirability ,(or desirability 1.0)))))
                                  ; Todo: I guess 1.0 is default?
    (ob$set goal 'episode ep)
    (ob$add-unique-name ep (string->symbol
     (string-append "EPISODE." (fixnum->string *next-ep-number*))))
    (increment-me *next-ep-number*)
    (setq *episodes* (cons ep *episodes*))
    ep))

;
; Episode storage and retrieval
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'make-episode',[rule,goal,context,realism,desirability],[let,[[ep,['ob$fcreate',['#BQ',['EPISODE',rule,['#COMMA',rule],goal,['#COMMA',goal],context,['#COMMA',context],realism,['#COMMA',[or,realism,[strength,['ob$get',goal,[quote,obj]]]]],desirability,['#COMMA',[or,desirability,1.0]]]]]]],['ob$set',goal,[quote,episode],ep],['ob$add-unique-name',ep,['string->symbol',['string-append','$STRING'("EPISODE."),['fixnum->string','*next-ep-number*']]]],['increment-me','*next-ep-number*'],[setq,'*episodes*',[cons,ep,'*episodes*']],ep]])
wl:lambda_def(defun, u_make_episode, f_u_make_episode, [u_rule, u_goal, u_context, u_realism, u_desirability], [[let, [[u_ep, [u_ob_c36_fcreate, ['#BQ', [u_episode, u_rule, ['#COMMA', u_rule], u_goal, ['#COMMA', u_goal], u_context, ['#COMMA', u_context], u_realism, ['#COMMA', [or, u_realism, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]]], u_desirability, ['#COMMA', [or, u_desirability, 1.0]]]]]]], [u_ob_c36_set, u_goal, [quote, u_episode], u_ep], [u_ob_c36_add_unique_name, u_ep, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "EPISODE."), [u_fixnum_c62_string, u_xx_next_ep_number_xx]]]], [u_increment_me, u_xx_next_ep_number_xx], [setq, u_xx_episodes_xx, [cons, u_ep, u_xx_episodes_xx]], u_ep]]).
wl:arglist_info(u_make_episode, f_u_make_episode, [u_rule, u_goal, u_context, u_realism, u_desirability], arginfo{all:[u_rule, u_goal, u_context, u_realism, u_desirability], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_goal, u_context, u_realism, u_desirability], opt:0, req:[u_rule, u_goal, u_context, u_realism, u_desirability], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_make_episode).

/*

### Compiled:  `U::MAKE-EPISODE` 
*/
f_u_make_episode(Rule, Goal, Context, Realism, Desirability, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(u_rule, Rule), bv(u_goal, Goal), bv(u_context, Context), bv(u_realism, Realism), bv(u_desirability, Desirability)|Env],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ u_episode,
			       u_rule,
			       ['#COMMA', u_rule],
			       u_goal,
			       ['#COMMA', u_goal],
			       u_context,
			       ['#COMMA', u_context],
			       u_realism,
			       
			       [ '#COMMA',
				 
				 [ or,
				   u_realism,
				   
				   [ u_strength,
				     [u_ob_c36_get, u_goal, [quote, u_obj]]
				   ]
				 ]
			       ],
			       u_desirability,
			       ['#COMMA', [or, u_desirability, 1.0]]
			     ]
			   ],
			   Ep_Init),
	LEnv=[bv(u_ep, Ep_Init)|Env17],
	get_var(LEnv, u_ep, Ep_Get),
	get_var(LEnv, u_goal, Goal_Get),
	f_u_ob_c36_set(Goal_Get, u_episode, Ep_Get, C36_set_Ret),
	get_var(LEnv, u_ep, Ep_Get10),
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*], claz_base_character, "EPISODE."),
				[u_fixnum_c62_string, u_xx_next_ep_number_xx]
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_add_unique_name(Ep_Get10, C62_symbol_Ret, Unique_name_Ret),
	f_u_increment_me(u_xx_next_ep_number_xx, Increment_me_Ret),
	get_var(LEnv, u_ep, Ep_Get12),
	get_var(LEnv, u_xx_episodes_xx, Xx_episodes_xx_Get),
	Xx_episodes_xx=[Ep_Get12|Xx_episodes_xx_Get],
	set_var(LEnv, u_xx_episodes_xx, Xx_episodes_xx),
	get_var(LEnv, u_ep, Ep_Get14),
	Ep_Get14=FnResult.
:- set_opv(f_u_make_episode, classof, claz_function),
   set_opv(u_make_episode, compile_as, kw_function),
   set_opv(u_make_episode, function, f_u_make_episode),
   DefunResult=u_make_episode.
/*
:- side_effect(assert_lsp(u_make_episode,
			  wl:lambda_def(defun, u_make_episode, f_u_make_episode, [u_rule, u_goal, u_context, u_realism, u_desirability], [[let, [[u_ep, [u_ob_c36_fcreate, ['#BQ', [u_episode, u_rule, ['#COMMA', u_rule], u_goal, ['#COMMA', u_goal], u_context, ['#COMMA', u_context], u_realism, ['#COMMA', [or, u_realism, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]]], u_desirability, ['#COMMA', [or, u_desirability, 1.0]]]]]]], [u_ob_c36_set, u_goal, [quote, u_episode], u_ep], [u_ob_c36_add_unique_name, u_ep, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "EPISODE."), [u_fixnum_c62_string, u_xx_next_ep_number_xx]]]], [u_increment_me, u_xx_next_ep_number_xx], [setq, u_xx_episodes_xx, [cons, u_ep, u_xx_episodes_xx]], u_ep]]))).
*/
/*
:- side_effect(assert_lsp(u_make_episode,
			  wl:arglist_info(u_make_episode, f_u_make_episode, [u_rule, u_goal, u_context, u_realism, u_desirability], arginfo{all:[u_rule, u_goal, u_context, u_realism, u_desirability], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_goal, u_context, u_realism, u_desirability], opt:0, req:[u_rule, u_goal, u_context, u_realism, u_desirability], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_make_episode,
			  wl:init_args(exact_only, f_u_make_episode))).
*/
/*
 Todo: I guess 1.0 is default?
*/
/*
*/
/*
 Episode storage and retrieval
*/
/*
*/
/*
(setq *episodic-memory* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2800 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*episodic-memory*',[]])
:- set_var(AEnv, setq, u_xx_episodic_memory_xx, []).
/*
(defun epmem-init ()
  (setq *episodic-memory* (cx$create))
  (ob$add-name *episodic-memory* 'episodic-memory)
  *episodic-memory*)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2830 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-init',[],[setq,'*episodic-memory*',['cx$create']],['ob$add-name','*episodic-memory*',[quote,'episodic-memory']],'*episodic-memory*'])
wl:lambda_def(defun, u_epmem_init, f_u_epmem_init, [], [[setq, u_xx_episodic_memory_xx, [u_cx_c36_create]], [u_ob_c36_add_name, u_xx_episodic_memory_xx, [quote, u_episodic_memory]], u_xx_episodic_memory_xx]).
wl:arglist_info(u_epmem_init, f_u_epmem_init, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_init).

/*

### Compiled:  `U::EPMEM-INIT` 
*/
f_u_epmem_init(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_cx_c36_create(Xx_episodic_memory_xx),
	set_var(AEnv, u_xx_episodic_memory_xx, Xx_episodic_memory_xx),
	get_var(AEnv, u_xx_episodic_memory_xx, Xx_episodic_memory_xx_Get),
	f_u_ob_c36_add_name(Xx_episodic_memory_xx_Get,
			    u_episodic_memory,
			    Episodic_memory),
	get_var(AEnv, u_xx_episodic_memory_xx, Xx_episodic_memory_xx_Get6),
	Xx_episodic_memory_xx_Get6=FnResult.
:- set_opv(f_u_epmem_init, classof, claz_function),
   set_opv(u_epmem_init, compile_as, kw_function),
   set_opv(u_epmem_init, function, f_u_epmem_init),
   DefunResult=u_epmem_init.
/*
:- side_effect(assert_lsp(u_epmem_init,
			  wl:lambda_def(defun, u_epmem_init, f_u_epmem_init, [], [[setq, u_xx_episodic_memory_xx, [u_cx_c36_create]], [u_ob_c36_add_name, u_xx_episodic_memory_xx, [quote, u_episodic_memory]], u_xx_episodic_memory_xx]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_init,
			  wl:arglist_info(u_epmem_init, f_u_epmem_init, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_init,
			  wl:init_args(exact_only, f_u_epmem_init))).
*/
/*
(defun epmem-initialize ()
  (setq *recent-episodes* nil)
  (setq *recent-indices* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2963 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-initialize',[],[setq,'*recent-episodes*',[]],[setq,'*recent-indices*',[]]])
wl:lambda_def(defun, u_epmem_initialize, f_u_epmem_initialize, [], [[setq, u_xx_recent_episodes_xx, []], [setq, u_xx_recent_indices_xx, []]]).
wl:arglist_info(u_epmem_initialize, f_u_epmem_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_initialize).

/*

### Compiled:  `U::EPMEM-INITIALIZE` 
*/
f_u_epmem_initialize(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	set_var(AEnv, setq, u_xx_recent_episodes_xx, []),
	set_var(AEnv, setq, u_xx_recent_indices_xx, []),
	[]=FnResult.
:- set_opv(f_u_epmem_initialize, classof, claz_function),
   set_opv(u_epmem_initialize, compile_as, kw_function),
   set_opv(u_epmem_initialize, function, f_u_epmem_initialize),
   DefunResult=u_epmem_initialize.
/*
:- side_effect(assert_lsp(u_epmem_initialize,
			  wl:lambda_def(defun, u_epmem_initialize, f_u_epmem_initialize, [], [[setq, u_xx_recent_episodes_xx, []], [setq, u_xx_recent_indices_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_initialize,
			  wl:arglist_info(u_epmem_initialize, f_u_epmem_initialize, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_initialize,
			  wl:init_args(exact_only, f_u_epmem_initialize))).
*/
/*
(defun epmem-store (episode index needed-for-plan? needed-for-reminding?)
  (ndbg-roman-nl *gate-dbg* ep-store "Storing "(defun epmem-store (episode index needed-for-plan? needed-for-reminding?)\n  (ndbg-roman-nl *gate-dbg* ep-store \"Storing ~A under ~A\" episode index)\n  (setq index (index-intern index 'new-ok))\n  (ob$add index 'indexes episode)\n  (ob$add episode 'indexed-under index)\n  (if needed-for-plan?\n      (ob$set episode 'plan-threshold\n              (+ 1 (or (ob$get episode 'plan-threshold) 0))))\n  (if needed-for-reminding?\n      (ob$set episode 'reminding-threshold\n              (+ 1 (or (ob$get episode 'reminding-threshold) 0)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3053 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-store',[episode,index,'needed-for-plan?','needed-for-reminding?'],['ndbg-roman-nl','*gate-dbg*','ep-store','$STRING'("Storing ~A under ~A"),episode,index],[setq,index,['index-intern',index,[quote,'new-ok']]],['ob$add',index,[quote,indexes],episode],['ob$add',episode,[quote,'indexed-under'],index],[if,'needed-for-plan?',['ob$set',episode,[quote,'plan-threshold'],[+,1,[or,['ob$get',episode,[quote,'plan-threshold']],0]]]],[if,'needed-for-reminding?',['ob$set',episode,[quote,'reminding-threshold'],[+,1,[or,['ob$get',episode,[quote,'reminding-threshold']],0]]]]])
wl:lambda_def(defun, u_epmem_store, f_u_epmem_store, [u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Storing ~A under ~A"), u_episode, index], [setq, index, [u_index_intern, index, [quote, u_new_ok]]], [u_ob_c36_add, index, [quote, u_indexes], u_episode], [u_ob_c36_add, u_episode, [quote, u_indexed_under], index], [if, u_needed_for_plan_c63, [u_ob_c36_set, u_episode, [quote, u_plan_threshold], [+, 1, [or, [u_ob_c36_get, u_episode, [quote, u_plan_threshold]], 0]]]], [if, u_needed_for_reminding_c63, [u_ob_c36_set, u_episode, [quote, u_reminding_threshold], [+, 1, [or, [u_ob_c36_get, u_episode, [quote, u_reminding_threshold]], 0]]]]]).
wl:arglist_info(u_epmem_store, f_u_epmem_store, [u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], arginfo{all:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], opt:0, req:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_store).

/*

### Compiled:  `U::EPMEM-STORE` 
*/
f_u_epmem_store(Episode, Index, Needed_for_plan_c63, Needed_for_reminding_c63, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_episode, Episode), bv(index, Index), bv(u_needed_for_plan_c63, Needed_for_plan_c63), bv(u_needed_for_reminding_c63, Needed_for_reminding_c63)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Storing ~A under ~A"),
			    u_episode,
			    index
			  ],
			  Roman_nl_Ret),
	get_var(AEnv, index, Index_Get),
	f_u_index_intern(Index_Get, u_new_ok, New_ok),
	set_var(AEnv, index, New_ok),
	get_var(AEnv, index, Index_Get6),
	get_var(AEnv, u_episode, Episode_Get),
	f_u_ob_c36_add(Index_Get6, u_indexes, Episode_Get, C36_add_Ret),
	get_var(AEnv, index, Index_Get9),
	get_var(AEnv, u_episode, Episode_Get8),
	f_u_ob_c36_add(Episode_Get8, u_indexed_under, Index_Get9, C36_add_Ret35),
	get_var(AEnv, u_needed_for_plan_c63, IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_episode, Episode_Get13),
	    (   get_var(AEnv, u_episode, Episode_Get14),
		f_u_ob_c36_get(Episode_Get14, u_plan_threshold, FORM1_Res),
		FORM1_Res\==[],
		_214188394=FORM1_Res
	    ->  true
	    ;   _214188394=0
	    ),
	    +(1, _214188394, Plan_threshold),
	    f_u_ob_c36_set(Episode_Get13,
			   u_plan_threshold,
			   Plan_threshold,
			   TrueResult),
	    _214182948=TrueResult
	;   _214182948=[]
	),
	get_var(AEnv, u_needed_for_reminding_c63, IFTEST17),
	(   IFTEST17\==[]
	->  get_var(AEnv, u_episode, Episode_Get20),
	    (   get_var(AEnv, u_episode, Episode_Get21),
		f_u_ob_c36_get(Episode_Get21,
			       u_reminding_threshold,
			       FORM1_Res22),
		FORM1_Res22\==[],
		_214199762=FORM1_Res22
	    ->  true
	    ;   _214199762=0
	    ),
	    +(1, _214199762, Reminding_threshold),
	    f_u_ob_c36_set(Episode_Get20,
			   u_reminding_threshold,
			   Reminding_threshold,
			   TrueResult23),
	    FnResult=TrueResult23
	;   FnResult=[]
	).
:- set_opv(f_u_epmem_store, classof, claz_function),
   set_opv(u_epmem_store, compile_as, kw_function),
   set_opv(u_epmem_store, function, f_u_epmem_store),
   DefunResult=u_epmem_store.
/*
:- side_effect(assert_lsp(u_epmem_store,
			  wl:lambda_def(defun, u_epmem_store, f_u_epmem_store, [u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Storing ~A under ~A"), u_episode, index], [setq, index, [u_index_intern, index, [quote, u_new_ok]]], [u_ob_c36_add, index, [quote, u_indexes], u_episode], [u_ob_c36_add, u_episode, [quote, u_indexed_under], index], [if, u_needed_for_plan_c63, [u_ob_c36_set, u_episode, [quote, u_plan_threshold], [+, 1, [or, [u_ob_c36_get, u_episode, [quote, u_plan_threshold]], 0]]]], [if, u_needed_for_reminding_c63, [u_ob_c36_set, u_episode, [quote, u_reminding_threshold], [+, 1, [or, [u_ob_c36_get, u_episode, [quote, u_reminding_threshold]], 0]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_store,
			  wl:arglist_info(u_epmem_store, f_u_epmem_store, [u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], arginfo{all:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], opt:0, req:[u_episode, index, u_needed_for_plan_c63, u_needed_for_reminding_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_store,
			  wl:init_args(exact_only, f_u_epmem_store))).
*/
/*
(setq *epmem-marks* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3582 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*epmem-marks*',[]])
:- set_var(AEnv, setq, u_xx_epmem_marks_xx, []).
/*
(defun mark-init ()
 (mark-unmark-all))
; currently as a safety precaution
;(setq *epmem-marks* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3608 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'mark-init',[],['mark-unmark-all']])
wl:lambda_def(defun, u_mark_init, f_u_mark_init, [], [[u_mark_unmark_all]]).
wl:arglist_info(u_mark_init, f_u_mark_init, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mark_init).

/*

### Compiled:  `U::MARK-INIT` 
*/
f_u_mark_init(FnResult) :-
	nop(global_env(Env)),
	_216524428=Env,
	f_u_mark_unmark_all(Unmark_all_Ret),
	Unmark_all_Ret=FnResult.
:- set_opv(f_u_mark_init, classof, claz_function),
   set_opv(u_mark_init, compile_as, kw_function),
   set_opv(u_mark_init, function, f_u_mark_init),
   DefunResult=u_mark_init.
/*
:- side_effect(assert_lsp(u_mark_init,
			  wl:lambda_def(defun, u_mark_init, f_u_mark_init, [], [[u_mark_unmark_all]]))).
*/
/*
:- side_effect(assert_lsp(u_mark_init,
			  wl:arglist_info(u_mark_init, f_u_mark_init, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mark_init, wl:init_args(exact_only, f_u_mark_init))).
*/
/*
 currently as a safety precaution
*/
/*
(setq *epmem-marks* nil)
*/
/*
(defun mark-unmark-all ()
  (yloop (yfor mark in *epmem-marks*)
         (ydo (ob$removes mark 'marks)))
; later change to replace slot value to 0 for
; less garbage.
  (setq *epmem-marks* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3710 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'mark-unmark-all',[],[yloop,[yfor,mark,in,'*epmem-marks*'],[ydo,['ob$removes',mark,[quote,marks]]]],[setq,'*epmem-marks*',[]]])
wl:lambda_def(defun, u_mark_unmark_all, f_u_mark_unmark_all, [], [[u_yloop, [u_yfor, u_mark, u_in, u_xx_epmem_marks_xx], [u_ydo, [u_ob_c36_removes, u_mark, [quote, u_marks]]]], [setq, u_xx_epmem_marks_xx, []]]).
wl:arglist_info(u_mark_unmark_all, f_u_mark_unmark_all, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mark_unmark_all).

/*

### Compiled:  `U::MARK-UNMARK-ALL` 
*/
f_u_mark_unmark_all(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_yloop(
		  [ [u_yfor, u_mark, u_in, u_xx_epmem_marks_xx],
		    [u_ydo, [u_ob_c36_removes, u_mark, [quote, u_marks]]]
		  ],
		  Yloop_Ret),
	set_var(AEnv, setq, u_xx_epmem_marks_xx, []),
	[]=FnResult.
:- set_opv(f_u_mark_unmark_all, classof, claz_function),
   set_opv(u_mark_unmark_all, compile_as, kw_function),
   set_opv(u_mark_unmark_all, function, f_u_mark_unmark_all),
   DefunResult=u_mark_unmark_all.
/*
:- side_effect(assert_lsp(u_mark_unmark_all,
			  wl:lambda_def(defun, u_mark_unmark_all, f_u_mark_unmark_all, [], [[u_yloop, [u_yfor, u_mark, u_in, u_xx_epmem_marks_xx], [u_ydo, [u_ob_c36_removes, u_mark, [quote, u_marks]]]], [setq, u_xx_epmem_marks_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_mark_unmark_all,
			  wl:arglist_info(u_mark_unmark_all, f_u_mark_unmark_all, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mark_unmark_all,
			  wl:init_args(exact_only, f_u_mark_unmark_all))).
*/
/*
 later change to replace slot value to 0 for
*/
/*
 less garbage.
*/
/*
(defun mark-mark (ob)
  (let ((marks (+ 1 (or (ob$get ob 'marks) 0))))
       (ob$set ob 'marks marks)
       (if (eq? marks 1)
           (setq *epmem-marks* (cons ob *epmem-marks*)))
       marks))

; This function is used to retrieve episodes in planning.
; epmem-reminding is later called on an episode returned by this
; procedure if in fact other heuristics decide to use that episode.
; (Thus only "appropriate" episodes are actually recalled in this case.)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'mark-mark',[ob],[let,[[marks,[+,1,[or,['ob$get',ob,[quote,marks]],0]]]],['ob$set',ob,[quote,marks],marks],[if,['eq?',marks,1],[setq,'*epmem-marks*',[cons,ob,'*epmem-marks*']]],marks]])
wl:lambda_def(defun, u_mark_mark, f_u_mark_mark, [u_ob], [[let, [[u_marks, [+, 1, [or, [u_ob_c36_get, u_ob, [quote, u_marks]], 0]]]], [u_ob_c36_set, u_ob, [quote, u_marks], u_marks], [if, [u_eq_c63, u_marks, 1], [setq, u_xx_epmem_marks_xx, [cons, u_ob, u_xx_epmem_marks_xx]]], u_marks]]).
wl:arglist_info(u_mark_mark, f_u_mark_mark, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mark_mark).

/*

### Compiled:  `U::MARK-MARK` 
*/
f_u_mark_mark(Ob, FnResult) :-
	nop(global_env(Env)),
	Env21=[bv(u_ob, Ob)|Env],
	(   get_var(Env21, u_ob, Ob_Get),
	    f_u_ob_c36_get(Ob_Get, u_marks, FORM1_Res),
	    FORM1_Res\==[],
	    _217878986=FORM1_Res
	->  true
	;   _217878986=0
	),
	+(1, _217878986, Marks_Init),
	LEnv=[bv(u_marks, Marks_Init)|Env21],
	get_var(LEnv, u_marks, Marks_Get),
	get_var(LEnv, u_ob, Ob_Get10),
	f_u_ob_c36_set(Ob_Get10, u_marks, Marks_Get, C36_set_Ret),
	f_u_eq_c63(u_marks, 1, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_ob, Ob_Get15),
	    get_var(LEnv, u_xx_epmem_marks_xx, Xx_epmem_marks_xx_Get),
	    TrueResult=[Ob_Get15|Xx_epmem_marks_xx_Get],
	    set_var(LEnv, u_xx_epmem_marks_xx, TrueResult),
	    _217903258=TrueResult
	;   _217903258=[]
	),
	get_var(LEnv, u_marks, Marks_Get18),
	Marks_Get18=FnResult.
:- set_opv(f_u_mark_mark, classof, claz_function),
   set_opv(u_mark_mark, compile_as, kw_function),
   set_opv(u_mark_mark, function, f_u_mark_mark),
   DefunResult=u_mark_mark.
/*
:- side_effect(assert_lsp(u_mark_mark,
			  wl:lambda_def(defun, u_mark_mark, f_u_mark_mark, [u_ob], [[let, [[u_marks, [+, 1, [or, [u_ob_c36_get, u_ob, [quote, u_marks]], 0]]]], [u_ob_c36_set, u_ob, [quote, u_marks], u_marks], [if, [u_eq_c63, u_marks, 1], [setq, u_xx_epmem_marks_xx, [cons, u_ob, u_xx_epmem_marks_xx]]], u_marks]]))).
*/
/*
:- side_effect(assert_lsp(u_mark_mark,
			  wl:arglist_info(u_mark_mark, f_u_mark_mark, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mark_mark, wl:init_args(exact_only, f_u_mark_mark))).
*/
/*
 This function is used to retrieve episodes in planning.
*/
/*
 epmem-reminding is later called on an episode returned by this
*/
/*
 procedure if in fact other heuristics decide to use that episode.
*/
/*
 (Thus only "appropriate" episodes are actually recalled in this case.)
*/
/*
(defun episode-retrieve (rule)
  (ndbg-roman-nl *gate-dbg* rule-xtra "Find potential episodes for "(defun episode-retrieve (rule)\n  (ndbg-roman-nl *gate-dbg* rule-xtra \"Find potential episodes for ~A\" rule)\n  (let ((new (epmem-retrieve1 (list rule) nil 'plan-threshold))\n        (result nil))\n       (yloop (yfor ep in (ob$gets rule 'indexes))\n              (ydo (if (or (memq? ep new)\n                           (recent-episode? ep))\n                       (setq result (cons ep result)))))\n       (if result (ndbg-roman-nl *gate-dbg* rule-xtra\n                                 \"Potential episodes = ~A\" result))\n       result))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4371 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-retrieve',[rule],['ndbg-roman-nl','*gate-dbg*','rule-xtra','$STRING'("Find potential episodes for ~A"),rule],[let,[[new,['epmem-retrieve1',[list,rule],[],[quote,'plan-threshold']]],[result,[]]],[yloop,[yfor,ep,in,['ob$gets',rule,[quote,indexes]]],[ydo,[if,[or,['memq?',ep,new],['recent-episode?',ep]],[setq,result,[cons,ep,result]]]]],[if,result,['ndbg-roman-nl','*gate-dbg*','rule-xtra','$STRING'("Potential episodes = ~A"),result]],result]])
wl:lambda_def(defun, u_episode_retrieve, f_u_episode_retrieve, [u_rule], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Find potential episodes for ~A"), u_rule], [let, [[u_new, [u_epmem_retrieve1, [list, u_rule], [], [quote, u_plan_threshold]]], [u_result, []]], [u_yloop, [u_yfor, u_ep, u_in, [u_ob_c36_gets, u_rule, [quote, u_indexes]]], [u_ydo, [if, [or, [u_memq_c63, u_ep, u_new], [u_recent_episode_c63, u_ep]], [setq, u_result, [cons, u_ep, u_result]]]]], [if, u_result, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Potential episodes = ~A"), u_result]], u_result]]).
wl:arglist_info(u_episode_retrieve, f_u_episode_retrieve, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_retrieve).

/*

### Compiled:  `U::EPISODE-RETRIEVE` 
*/
f_u_episode_retrieve(Rule, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_rule, Rule)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Find potential episodes for ~A"),
			    u_rule
			  ],
			  Roman_nl_Ret),
	get_var(Env16, u_rule, Rule_Get),
	Epmem_retrieve1_Param=[Rule_Get],
	f_u_epmem_retrieve1(Epmem_retrieve1_Param,
			    [],
			    u_plan_threshold,
			    New_Init),
	LEnv=[bv(u_new, New_Init), bv(u_result, [])|Env16],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ep,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_indexes]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ or,
			  [u_memq_c63, u_ep, u_new],
			  [u_recent_episode_c63, u_ep]
			],
			[setq, u_result, [cons, u_ep, u_result]]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_result, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_xtra,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Potential episodes = ~A"),
				u_result
			      ],
			      TrueResult),
	    _219171886=TrueResult
	;   _219171886=[]
	),
	get_var(LEnv, u_result, Result_Get13),
	Result_Get13=FnResult.
:- set_opv(f_u_episode_retrieve, classof, claz_function),
   set_opv(u_episode_retrieve, compile_as, kw_function),
   set_opv(u_episode_retrieve, function, f_u_episode_retrieve),
   DefunResult=u_episode_retrieve.
/*
:- side_effect(assert_lsp(u_episode_retrieve,
			  wl:lambda_def(defun, u_episode_retrieve, f_u_episode_retrieve, [u_rule], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Find potential episodes for ~A"), u_rule], [let, [[u_new, [u_epmem_retrieve1, [list, u_rule], [], [quote, u_plan_threshold]]], [u_result, []]], [u_yloop, [u_yfor, u_ep, u_in, [u_ob_c36_gets, u_rule, [quote, u_indexes]]], [u_ydo, [if, [or, [u_memq_c63, u_ep, u_new], [u_recent_episode_c63, u_ep]], [setq, u_result, [cons, u_ep, u_result]]]]], [if, u_result, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Potential episodes = ~A"), u_result]], u_result]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_retrieve,
			  wl:arglist_info(u_episode_retrieve, f_u_episode_retrieve, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_retrieve,
			  wl:init_args(exact_only, f_u_episode_retrieve))).
*/
/*
(defun epmem-retrieve (indices serendipity? threshold-type)
  (let ((eps (epmem-retrieve1 indices serendipity? threshold-type)))
       (yloop
        (yfor ep in eps)
        (ydo (epmem-reminding ep nil nil)))
       eps))

; If serendipity? is T, then one less the normal threshold (kind specified
; by threshold-type) will result in retrieval (since serendipity can be
; thought of as providing an extra index).
; This does NOT retrieve episodes that are already recent.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-retrieve',[indices,'serendipity?','threshold-type'],[let,[[eps,['epmem-retrieve1',indices,'serendipity?','threshold-type']]],[yloop,[yfor,ep,in,eps],[ydo,['epmem-reminding',ep,[],[]]]],eps]])
wl:lambda_def(defun, u_epmem_retrieve, f_u_epmem_retrieve, [u_indices, u_serendipity_c63, u_threshold_type], [[let, [[u_eps, [u_epmem_retrieve1, u_indices, u_serendipity_c63, u_threshold_type]]], [u_yloop, [u_yfor, u_ep, u_in, u_eps], [u_ydo, [u_epmem_reminding, u_ep, [], []]]], u_eps]]).
wl:arglist_info(u_epmem_retrieve, f_u_epmem_retrieve, [u_indices, u_serendipity_c63, u_threshold_type], arginfo{all:[u_indices, u_serendipity_c63, u_threshold_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_indices, u_serendipity_c63, u_threshold_type], opt:0, req:[u_indices, u_serendipity_c63, u_threshold_type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_retrieve).

/*

### Compiled:  `U::EPMEM-RETRIEVE` 
*/
f_u_epmem_retrieve(Indices, Serendipity_c63, Threshold_type, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_indices, Indices), bv(u_serendipity_c63, Serendipity_c63), bv(u_threshold_type, Threshold_type)|Env],
	get_var(Env14, u_indices, Indices_Get),
	get_var(Env14, u_serendipity_c63, Serendipity_c63_Get),
	get_var(Env14, u_threshold_type, Threshold_type_Get),
	f_u_epmem_retrieve1(Indices_Get,
			    Serendipity_c63_Get,
			    Threshold_type_Get,
			    Eps_Init),
	LEnv=[bv(u_eps, Eps_Init)|Env14],
	f_u_yloop(
		  [ [u_yfor, u_ep, u_in, u_eps],
		    [u_ydo, [u_epmem_reminding, u_ep, [], []]]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_eps, Eps_Get),
	Eps_Get=FnResult.
:- set_opv(f_u_epmem_retrieve, classof, claz_function),
   set_opv(u_epmem_retrieve, compile_as, kw_function),
   set_opv(u_epmem_retrieve, function, f_u_epmem_retrieve),
   DefunResult=u_epmem_retrieve.
/*
:- side_effect(assert_lsp(u_epmem_retrieve,
			  wl:lambda_def(defun, u_epmem_retrieve, f_u_epmem_retrieve, [u_indices, u_serendipity_c63, u_threshold_type], [[let, [[u_eps, [u_epmem_retrieve1, u_indices, u_serendipity_c63, u_threshold_type]]], [u_yloop, [u_yfor, u_ep, u_in, u_eps], [u_ydo, [u_epmem_reminding, u_ep, [], []]]], u_eps]]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_retrieve,
			  wl:arglist_info(u_epmem_retrieve, f_u_epmem_retrieve, [u_indices, u_serendipity_c63, u_threshold_type], arginfo{all:[u_indices, u_serendipity_c63, u_threshold_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_indices, u_serendipity_c63, u_threshold_type], opt:0, req:[u_indices, u_serendipity_c63, u_threshold_type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_retrieve,
			  wl:init_args(exact_only, f_u_epmem_retrieve))).
*/
/*
 If serendipity? is T, then one less the normal threshold (kind specified
*/
/*
 by threshold-type) will result in retrieval (since serendipity can be
*/
/*
 thought of as providing an extra index).
*/
/*
 This does NOT retrieve episodes that are already recent.
*/
/*
(defun epmem-retrieve1 (indices serendipity? threshold-type)
  (mark-init)
  (yloop
   (initial (result nil)
            (marks nil)
            (threshx nil))
   (yfor index in indices)
   (ydo
    (if (setq index (index-intern index 'old))
        (yloop
         (yfor episode in (ob$gets index 'indexes))
         (ydo (if (not (recent-episode? episode))
                 (progn
                  (setq marks (mark-mark episode))
                  (ndbg-roman-nl *gate-dbg* remind ""(defun epmem-retrieve1 (indices serendipity? threshold-type)\n  (mark-init)\n  (yloop\n   (initial (result nil)\n            (marks nil)\n            (threshx nil))\n   (yfor index in indices)\n   (ydo\n    (if (setq index (index-intern index 'old))\n        (yloop\n         (yfor episode in (ob$gets index 'indexes))\n         (ydo (if (not (recent-episode? episode))\n                 (progn\n                  (setq marks (mark-mark episode))\n                  (ndbg-roman-nl *gate-dbg* remind \"~A marks on ~A\"\n                                 marks episode)\n                  (setq threshx (if serendipity?\n                                  (- (ob$get episode threshold-type) 1)\n                                  (ob$get episode threshold-type)))\n                  (ndbg-roman-nl *gate-dbg* remind \"Net thresh for ~A is ~A\"\n                                 episode threshx)\n                  (cond\n                   ((= marks threshx)\n                    (setq result (cons episode result)))\n                   ((> marks threshx)\n                    (if (memq? episode result)\n                        (ndbg-roman-nl *gate-dbg* remind\n                                       \"Overdetermined epmem-retrieve ~A\"\n                                       episode)\n                        (setq result (cons episode result)))))))))))\n    (yresult (progn\n            (mark-unmark-all)\n            (if result (ndbg-roman-nl *gate-dbg* remind\n                                      \"epmem-retrieve1 returns ~A\"\n                                      result))\n            result))))\n\n; Assumes index is already copied if this is necessary.\n; new? = 'new-ok if it is OK to create a new index, else 'old\n; Note that rule indices don't get asserted in *episodic-memory*.\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:5378 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-retrieve1',[indices,'serendipity?','threshold-type'],['mark-init'],[yloop,[initial,[result,[]],[marks,[]],[threshx,[]]],[yfor,index,in,indices],[ydo,[if,[setq,index,['index-intern',index,[quote,old]]],[yloop,[yfor,episode,in,['ob$gets',index,[quote,indexes]]],[ydo,[if,[not,['recent-episode?',episode]],[progn,[setq,marks,['mark-mark',episode]],['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("~A marks on ~A"),marks,episode],[setq,threshx,[if,'serendipity?',[-,['ob$get',episode,'threshold-type'],1],['ob$get',episode,'threshold-type']]],['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("Net thresh for ~A is ~A"),episode,threshx],[cond,[[=,marks,threshx],[setq,result,[cons,episode,result]]],[[>,marks,threshx],[if,['memq?',episode,result],['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("Overdetermined epmem-retrieve ~A"),episode],[setq,result,[cons,episode,result]]]]]]]]]]],[yresult,[progn,['mark-unmark-all'],[if,result,['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("epmem-retrieve1 returns ~A"),result]],result]]]])
wl:lambda_def(defun, u_epmem_retrieve1, f_u_epmem_retrieve1, [u_indices, u_serendipity_c63, u_threshold_type], [[u_mark_init], [u_yloop, [u_initial, [u_result, []], [u_marks, []], [u_threshx, []]], [u_yfor, index, u_in, u_indices], [u_ydo, [if, [setq, index, [u_index_intern, index, [quote, u_old]]], [u_yloop, [u_yfor, u_episode, u_in, [u_ob_c36_gets, index, [quote, u_indexes]]], [u_ydo, [if, [not, [u_recent_episode_c63, u_episode]], [progn, [setq, u_marks, [u_mark_mark, u_episode]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "~A marks on ~A"), u_marks, u_episode], [setq, u_threshx, [if, u_serendipity_c63, [-, [u_ob_c36_get, u_episode, u_threshold_type], 1], [u_ob_c36_get, u_episode, u_threshold_type]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Net thresh for ~A is ~A"), u_episode, u_threshx], [cond, [[=, u_marks, u_threshx], [setq, u_result, [cons, u_episode, u_result]]], [[>, u_marks, u_threshx], [if, [u_memq_c63, u_episode, u_result], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Overdetermined epmem-retrieve ~A"), u_episode], [setq, u_result, [cons, u_episode, u_result]]]]]]]]]]], [u_yresult, [progn, [u_mark_unmark_all], [if, u_result, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "epmem-retrieve1 returns ~A"), u_result]], u_result]]]]).
wl:arglist_info(u_epmem_retrieve1, f_u_epmem_retrieve1, [u_indices, u_serendipity_c63, u_threshold_type], arginfo{all:[u_indices, u_serendipity_c63, u_threshold_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_indices, u_serendipity_c63, u_threshold_type], opt:0, req:[u_indices, u_serendipity_c63, u_threshold_type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_retrieve1).

/*

### Compiled:  `U::EPMEM-RETRIEVE1` 
*/
f_u_epmem_retrieve1(Indices, Serendipity_c63, Threshold_type, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_indices, Indices), bv(u_serendipity_c63, Serendipity_c63), bv(u_threshold_type, Threshold_type)|Env],
	f_u_mark_init(Mark_init_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_marks, []], [u_threshx, []]],
		    [u_yfor, index, u_in, u_indices],
		    
		    [ u_ydo,
		      
		      [ if,
			[setq, index, [u_index_intern, index, [quote, u_old]]],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_episode,
			    u_in,
			    [u_ob_c36_gets, index, [quote, u_indexes]]
			  ],
			  
			  [ u_ydo,
			    
			    [ if,
			      [not, [u_recent_episode_c63, u_episode]],
			      
			      [ progn,
				[setq, u_marks, [u_mark_mark, u_episode]],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_remind,
				  '$ARRAY'([*],
					   claz_base_character,
					   "~A marks on ~A"),
				  u_marks,
				  u_episode
				],
				
				[ setq,
				  u_threshx,
				  
				  [ if,
				    u_serendipity_c63,
				    
				    [ (-),
				      
				      [ u_ob_c36_get,
					u_episode,
					u_threshold_type
				      ],
				      1
				    ],
				    [u_ob_c36_get, u_episode, u_threshold_type]
				  ]
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_remind,
				  '$ARRAY'([*],
					   claz_base_character,
					   "Net thresh for ~A is ~A"),
				  u_episode,
				  u_threshx
				],
				
				[ cond,
				  
				  [ [=, u_marks, u_threshx],
				    [setq, u_result, [cons, u_episode, u_result]]
				  ],
				  
				  [ [>, u_marks, u_threshx],
				    
				    [ if,
				      [u_memq_c63, u_episode, u_result],
				      
				      [ u_ndbg_roman_nl,
					u_xx_gate_dbg_xx,
					u_remind,
					'$ARRAY'([*],
						 claz_base_character,
						 "Overdetermined epmem-retrieve ~A"),
					u_episode
				      ],
				      
				      [ setq,
					u_result,
					[cons, u_episode, u_result]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			[u_mark_unmark_all],
			
			[ if,
			  u_result,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_remind,
			    '$ARRAY'([*],
				     claz_base_character,
				     "epmem-retrieve1 returns ~A"),
			    u_result
			  ]
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_epmem_retrieve1, classof, claz_function),
   set_opv(u_epmem_retrieve1, compile_as, kw_function),
   set_opv(u_epmem_retrieve1, function, f_u_epmem_retrieve1),
   DefunResult=u_epmem_retrieve1.
/*
:- side_effect(assert_lsp(u_epmem_retrieve1,
			  wl:lambda_def(defun, u_epmem_retrieve1, f_u_epmem_retrieve1, [u_indices, u_serendipity_c63, u_threshold_type], [[u_mark_init], [u_yloop, [u_initial, [u_result, []], [u_marks, []], [u_threshx, []]], [u_yfor, index, u_in, u_indices], [u_ydo, [if, [setq, index, [u_index_intern, index, [quote, u_old]]], [u_yloop, [u_yfor, u_episode, u_in, [u_ob_c36_gets, index, [quote, u_indexes]]], [u_ydo, [if, [not, [u_recent_episode_c63, u_episode]], [progn, [setq, u_marks, [u_mark_mark, u_episode]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "~A marks on ~A"), u_marks, u_episode], [setq, u_threshx, [if, u_serendipity_c63, [-, [u_ob_c36_get, u_episode, u_threshold_type], 1], [u_ob_c36_get, u_episode, u_threshold_type]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Net thresh for ~A is ~A"), u_episode, u_threshx], [cond, [[=, u_marks, u_threshx], [setq, u_result, [cons, u_episode, u_result]]], [[>, u_marks, u_threshx], [if, [u_memq_c63, u_episode, u_result], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Overdetermined epmem-retrieve ~A"), u_episode], [setq, u_result, [cons, u_episode, u_result]]]]]]]]]]], [u_yresult, [progn, [u_mark_unmark_all], [if, u_result, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "epmem-retrieve1 returns ~A"), u_result]], u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_retrieve1,
			  wl:arglist_info(u_epmem_retrieve1, f_u_epmem_retrieve1, [u_indices, u_serendipity_c63, u_threshold_type], arginfo{all:[u_indices, u_serendipity_c63, u_threshold_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_indices, u_serendipity_c63, u_threshold_type], opt:0, req:[u_indices, u_serendipity_c63, u_threshold_type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_retrieve1,
			  wl:init_args(exact_only, f_u_epmem_retrieve1))).
*/
/*
 Assumes index is already copied if this is necessary.
*/
/*
 new? = 'new-ok if it is OK to create a new index, else 'old
*/
/*
 Note that rule indices don't get asserted in *episodic-memory*.
*/
/*
(defun index-intern (index new?)
  (if (ty$instance? index 'rule)
      index
      (let ((found (cx$retrieve *episodic-memory* index)))
           ; Retrieve returns a list of bindings, where the car of each
           ; is not T but rather the retrieved ob.
           (if found
               (caar found)
               (if (eq? new? 'new-ok)
                   (progn
                    (cx$assert *episodic-memory* index)
                    index)
                   nil)))))

;
; Reminding mechanism
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'index-intern',[index,'new?'],[if,['ty$instance?',index,[quote,rule]],index,[let,[[found,['cx$retrieve','*episodic-memory*',index]]],[if,found,[caar,found],[if,['eq?','new?',[quote,'new-ok']],[progn,['cx$assert','*episodic-memory*',index],index],[]]]]]])
wl:lambda_def(defun, u_index_intern, f_u_index_intern, [index, u_new_c63], [[if, [u_ty_c36_instance_c63, index, [quote, u_rule]], index, [let, [[u_found, [u_cx_c36_retrieve, u_xx_episodic_memory_xx, index]]], [if, u_found, [caar, u_found], [if, [u_eq_c63, u_new_c63, [quote, u_new_ok]], [progn, [u_cx_c36_assert, u_xx_episodic_memory_xx, index], index], []]]]]]).
wl:arglist_info(u_index_intern, f_u_index_intern, [index, u_new_c63], arginfo{all:[index, u_new_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[index, u_new_c63], opt:0, req:[index, u_new_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_index_intern).

/*

### Compiled:  `U::INDEX-INTERN` 
*/
f_u_index_intern(Index, New_c63, LetResult) :-
	nop(global_env(Env)),
	Env30=[bv(index, Index), bv(u_new_c63, New_c63)|Env],
	get_var(Env30, index, Index_Get),
	f_u_ty_c36_instance_c63(Index_Get, u_rule, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env30, index, Index_Get7),
	    LetResult=Index_Get7
	;   get_var(Env30, index, Index_Get12),
	    get_var(Env30, u_xx_episodic_memory_xx, Xx_episodic_memory_xx_Get),
	    f_u_cx_c36_retrieve(Xx_episodic_memory_xx_Get,
				Index_Get12,
				Found_Init),
	    LEnv=[bv(u_found, Found_Init)|Env30],
	    get_var(LEnv, u_found, IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(LEnv, u_found, Found_Get17),
		cl_caar(Found_Get17, TrueResult24),
		LetResult=TrueResult24
	    ;   f_u_eq_c63(u_new_c63, [quote, u_new_ok], IFTEST18),
		(   IFTEST18\==[]
		->  get_var(LEnv, index, Index_Get21),
		    get_var(LEnv,
			    u_xx_episodic_memory_xx,
			    Xx_episodic_memory_xx_Get20),
		    f_u_cx_c36_assert(Xx_episodic_memory_xx_Get20,
				      Index_Get21,
				      C36_assert_Ret),
		    get_var(LEnv, index, Index_Get22),
		    LetResult=Index_Get22
		;   LetResult=[]
		)
	    )
	).
:- set_opv(f_u_index_intern, classof, claz_function),
   set_opv(u_index_intern, compile_as, kw_function),
   set_opv(u_index_intern, function, f_u_index_intern),
   DefunResult=u_index_intern.
/*
:- side_effect(assert_lsp(u_index_intern,
			  wl:lambda_def(defun, u_index_intern, f_u_index_intern, [index, u_new_c63], [[if, [u_ty_c36_instance_c63, index, [quote, u_rule]], index, [let, [[u_found, [u_cx_c36_retrieve, u_xx_episodic_memory_xx, index]]], [if, u_found, [caar, u_found], [if, [u_eq_c63, u_new_c63, [quote, u_new_ok]], [progn, [u_cx_c36_assert, u_xx_episodic_memory_xx, index], index], []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_index_intern,
			  wl:arglist_info(u_index_intern, f_u_index_intern, [index, u_new_c63], arginfo{all:[index, u_new_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[index, u_new_c63], opt:0, req:[index, u_new_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_index_intern,
			  wl:init_args(exact_only, f_u_index_intern))).
*/
/*
 Retrieve returns a list of bindings, where the car of each
*/
/*
 is not T but rather the retrieved ob.
*/
/*
*/
/*
 Reminding mechanism
*/
/*
*/
/*
(setq *recent-indices* nil)

; Todo: get rid of the superfluous consing.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7636 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*recent-indices*',[]])
:- set_var(AEnv, setq, u_xx_recent_indices_xx, []).
/*
 Todo: get rid of the superfluous consing.
*/
/*
(defun remindings ()
  (epmem-retrieve (append *recent-indices* 
                          (get-emotion-indices))
                  nil 'reminding-threshold))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7709 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,remindings,[],['epmem-retrieve',[append,'*recent-indices*',['get-emotion-indices']],[],[quote,'reminding-threshold']]])
wl:lambda_def(defun, u_remindings, f_u_remindings, [], [[u_epmem_retrieve, [append, u_xx_recent_indices_xx, [u_get_emotion_indices]], [], [quote, u_reminding_threshold]]]).
wl:arglist_info(u_remindings, f_u_remindings, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_remindings).

/*

### Compiled:  `U::REMINDINGS` 
*/
f_u_remindings(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, u_xx_recent_indices_xx, Xx_recent_indices_xx_Get),
	f_u_get_emotion_indices(Emotion_indices_Ret),
	cl_append(Xx_recent_indices_xx_Get,
		  Emotion_indices_Ret,
		  Epmem_retrieve_Param),
	f_u_epmem_retrieve(Epmem_retrieve_Param,
			   [],
			   u_reminding_threshold,
			   Reminding_threshold),
	Reminding_threshold=FnResult.
:- set_opv(f_u_remindings, classof, claz_function),
   set_opv(u_remindings, compile_as, kw_function),
   set_opv(u_remindings, function, f_u_remindings),
   DefunResult=u_remindings.
/*
:- side_effect(assert_lsp(u_remindings,
			  wl:lambda_def(defun, u_remindings, f_u_remindings, [], [[u_epmem_retrieve, [append, u_xx_recent_indices_xx, [u_get_emotion_indices]], [], [quote, u_reminding_threshold]]]))).
*/
/*
:- side_effect(assert_lsp(u_remindings,
			  wl:arglist_info(u_remindings, f_u_remindings, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_remindings,
			  wl:init_args(exact_only, f_u_remindings))).
*/
/*
(setq *pos-emot-ptn* (ob$fcreate '(POS-EMOTION)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7869 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*pos-emot-ptn*',['ob$fcreate',[quote,['POS-EMOTION']]]])
:- f_u_ob_c36_fcreate([quote, [u_pos_emotion]], _Ignored),
   set_var(AEnv, u_xx_pos_emot_ptn_xx, _Ignored).
/*
(setq *neg-emot-ptn* (ob$fcreate '(NEG-EMOTION)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7919 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*neg-emot-ptn*',['ob$fcreate',[quote,['NEG-EMOTION']]]])
:- f_u_ob_c36_fcreate([quote, [u_neg_emotion]], _Ignored),
   set_var(AEnv, u_xx_neg_emot_ptn_xx, _Ignored).
/*
(setq *pos-neg-list* (list *pos-emot-ptn* *neg-emot-ptn*))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7969 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*pos-neg-list*',[list,'*pos-emot-ptn*','*neg-emot-ptn*']])
:- get_var(AEnv, u_xx_neg_emot_ptn_xx, Xx_neg_emot_ptn_xx_Get),
   get_var(AEnv, u_xx_pos_emot_ptn_xx, Xx_pos_emot_ptn_xx_Get),
   _Ignored=[Xx_pos_emot_ptn_xx_Get, Xx_neg_emot_ptn_xx_Get],
   set_var(AEnv, u_xx_pos_neg_list_xx, _Ignored).
/*
(setq *pos-list* (list *pos-emot-ptn*))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8028 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*pos-list*',[list,'*pos-emot-ptn*']])
:- get_var(AEnv, u_xx_pos_emot_ptn_xx, Xx_pos_emot_ptn_xx_Get),
   _Ignored=[Xx_pos_emot_ptn_xx_Get],
   set_var(AEnv, u_xx_pos_list_xx, _Ignored).
/*
(setq *neg-list* (list *neg-emot-ptn*))

; Todo: in the future, we would like to index on the "quality" of the
; emotion (e.g., embarrassment) in addition to the sign.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8068 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*neg-list*',[list,'*neg-emot-ptn*']])
:- get_var(AEnv, u_xx_neg_emot_ptn_xx, Xx_neg_emot_ptn_xx_Get),
   _Ignored=[Xx_neg_emot_ptn_xx_Get],
   set_var(AEnv, u_xx_neg_list_xx, _Ignored).
/*
 Todo: in the future, we would like to index on the "quality" of the
*/
/*
 emotion (e.g., embarrassment) in addition to the sign.
*/
/*
(defun get-emotion-indices ()
  (cond
   ((fl> *overall-emotional-state* 1.0) *pos-list*)
   ((fl< *overall-emotional-state* -1.0) *neg-list*)
   (else nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8236 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'get-emotion-indices',[],[cond,[['fl>','*overall-emotional-state*',1.0],'*pos-list*'],[['fl<','*overall-emotional-state*',-1.0],'*neg-list*'],[else,[]]]])
wl:lambda_def(defun, u_get_emotion_indices, f_u_get_emotion_indices, [], [[cond, [[u_fl_c62, u_xx_overall_emotional_state_xx, 1.0], u_xx_pos_list_xx], [[u_fl_c60, u_xx_overall_emotional_state_xx, -1.0], u_xx_neg_list_xx], [u_else, []]]]).
wl:arglist_info(u_get_emotion_indices, f_u_get_emotion_indices, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_get_emotion_indices).

/*

### Compiled:  `U::GET-EMOTION-INDICES` 
*/
f_u_get_emotion_indices(ElseResult15) :-
	nop(global_env(Env)),
	GEnv=Env,
	f_u_fl_c62(u_xx_overall_emotional_state_xx, 1.0, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, u_xx_pos_list_xx, Xx_pos_list_xx_Get),
	    ElseResult15=Xx_pos_list_xx_Get
	;   f_u_fl_c60(u_xx_overall_emotional_state_xx, -1.0, IFTEST7),
	    (   IFTEST7\==[]
	    ->  get_var(GEnv, u_xx_neg_list_xx, Xx_neg_list_xx_Get),
		ElseResult15=Xx_neg_list_xx_Get
	    ;   get_var(GEnv, u_else, IFTEST10),
		(   IFTEST10\==[]
		->  ElseResult15=[]
		;   ElseResult15=[]
		)
	    )
	).
:- set_opv(f_u_get_emotion_indices, classof, claz_function),
   set_opv(u_get_emotion_indices, compile_as, kw_function),
   set_opv(u_get_emotion_indices, function, f_u_get_emotion_indices),
   DefunResult=u_get_emotion_indices.
/*
:- side_effect(assert_lsp(u_get_emotion_indices,
			  wl:lambda_def(defun, u_get_emotion_indices, f_u_get_emotion_indices, [], [[cond, [[u_fl_c62, u_xx_overall_emotional_state_xx, 1.0], u_xx_pos_list_xx], [[u_fl_c60, u_xx_overall_emotional_state_xx, -1.0], u_xx_neg_list_xx], [u_else, []]]]))).
*/
/*
:- side_effect(assert_lsp(u_get_emotion_indices,
			  wl:arglist_info(u_get_emotion_indices, f_u_get_emotion_indices, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_get_emotion_indices,
			  wl:init_args(exact_only, f_u_get_emotion_indices))).
*/
/*
(setq *recent-index-max-length* 6)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8396 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*recent-index-max-length*',6])
:- set_var(AEnv, setq, u_xx_recent_index_max_length_xx, 6).
/*
(defun add-recent-index (index)
  (if (not (memq? index *recent-indices*))
      (progn
       (ndbg-roman-nl *gate-dbg* remind "Activate index "(defun add-recent-index (index)\n  (if (not (memq? index *recent-indices*))\n      (progn\n       (ndbg-roman-nl *gate-dbg* remind \"Activate index ~A\" index)\n       (if (>= (length *recent-indices*) *recent-index-max-length*)\n           (progn\n            (ndbg-roman-nl *gate-dbg* remind \"Index ~A fades\"\n                           (car *recent-indices*))\n            (setq *recent-indices* (cdr *recent-indices*))))\n       (setq *recent-indices* (append! *recent-indices* (list index))))\n      (ndbg-roman-nl *gate-dbg* remind \"Index ~A already active\"\n                     index)))\n\n;\n; Environmental object input\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8432 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-recent-index',[index],[if,[not,['memq?',index,'*recent-indices*']],[progn,['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("Activate index ~A"),index],[if,[>=,[length,'*recent-indices*'],'*recent-index-max-length*'],[progn,['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("Index ~A fades"),[car,'*recent-indices*']],[setq,'*recent-indices*',[cdr,'*recent-indices*']]]],[setq,'*recent-indices*',['append!','*recent-indices*',[list,index]]]],['ndbg-roman-nl','*gate-dbg*',remind,'$STRING'("Index ~A already active"),index]]])
wl:lambda_def(defun, u_add_recent_index, f_u_add_recent_index, [index], [[if, [not, [u_memq_c63, index, u_xx_recent_indices_xx]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Activate index ~A"), index], [if, [>=, [length, u_xx_recent_indices_xx], u_xx_recent_index_max_length_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Index ~A fades"), [car, u_xx_recent_indices_xx]], [setq, u_xx_recent_indices_xx, [cdr, u_xx_recent_indices_xx]]]], [setq, u_xx_recent_indices_xx, [u_append_c33, u_xx_recent_indices_xx, [list, index]]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Index ~A already active"), index]]]).
wl:arglist_info(u_add_recent_index, f_u_add_recent_index, [index], arginfo{all:[index], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[index], opt:0, req:[index], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_add_recent_index).

/*

### Compiled:  `U::ADD-RECENT-INDEX` 
*/
f_u_add_recent_index(Index, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(index, Index)|Env],
	f_u_memq_c63(index, u_xx_recent_indices_xx, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_remind,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Activate index ~A"),
				index
			      ],
			      Roman_nl_Ret),
	    get_var(AEnv, u_xx_recent_indices_xx, Xx_recent_indices_xx_Get),
	    cl_length(Xx_recent_indices_xx_Get, PredArg1Result),
	    get_var(AEnv,
		    u_xx_recent_index_max_length_xx,
		    Xx_recent_index_max_length_xx_Get),
	    (   PredArg1Result>=Xx_recent_index_max_length_xx_Get
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_remind,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "Index ~A fades"),
				    [car, u_xx_recent_indices_xx]
				  ],
				  Roman_nl_Ret22),
		get_var(AEnv,
			u_xx_recent_indices_xx,
			Xx_recent_indices_xx_Get14),
		cl_cdr(Xx_recent_indices_xx_Get14, TrueResult),
		set_var(AEnv, u_xx_recent_indices_xx, TrueResult),
		_229195698=TrueResult
	    ;   _229195698=[]
	    ),
	    f_u_append_c33(u_xx_recent_indices_xx, [list, index], TrueResult16),
	    set_var(AEnv, u_xx_recent_indices_xx, TrueResult16),
	    FnResult=TrueResult16
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_remind,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Index ~A already active"),
				index
			      ],
			      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_recent_index, classof, claz_function),
   set_opv(u_add_recent_index, compile_as, kw_function),
   set_opv(u_add_recent_index, function, f_u_add_recent_index),
   DefunResult=u_add_recent_index.
/*
:- side_effect(assert_lsp(u_add_recent_index,
			  wl:lambda_def(defun, u_add_recent_index, f_u_add_recent_index, [index], [[if, [not, [u_memq_c63, index, u_xx_recent_indices_xx]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Activate index ~A"), index], [if, [>=, [length, u_xx_recent_indices_xx], u_xx_recent_index_max_length_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Index ~A fades"), [car, u_xx_recent_indices_xx]], [setq, u_xx_recent_indices_xx, [cdr, u_xx_recent_indices_xx]]]], [setq, u_xx_recent_indices_xx, [u_append_c33, u_xx_recent_indices_xx, [list, index]]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_remind, '$ARRAY'([*], claz_base_character, "Index ~A already active"), index]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_recent_index,
			  wl:arglist_info(u_add_recent_index, f_u_add_recent_index, [index], arginfo{all:[index], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[index], opt:0, req:[index], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_recent_index,
			  wl:init_args(exact_only, f_u_add_recent_index))).
*/
/*
*/
/*
 Environmental object input
*/
/*
*/
/*
(defun environmental-object-input ()
  (ndbg-roman-nl *gate-dbg* rule
                 "Taking optional object or concept input")
  (let ((concepts (enter-concepts *reality* *me-belief-path*)))
       (if (null? concepts)
           nil
           (let ((result1 (run-object-serendipities concepts))
                 (result2 (entered-concept-serendipity)))
                (or result1 result2)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9049 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'environmental-object-input',[],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Taking optional object or concept input")],[let,[[concepts,['enter-concepts','*reality*','*me-belief-path*']]],[if,['null?',concepts],[],[let,[[result1,['run-object-serendipities',concepts]],[result2,['entered-concept-serendipity']]],[or,result1,result2]]]]])
wl:lambda_def(defun, u_environmental_object_input, f_u_environmental_object_input, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Taking optional object or concept input")], [let, [[u_concepts, [u_enter_concepts, u_xx_reality_xx, u_xx_me_belief_path_xx]]], [if, [u_null_c63, u_concepts], [], [let, [[u_result1, [u_run_object_serendipities, u_concepts]], [u_result2, [u_entered_concept_serendipity]]], [or, u_result1, u_result2]]]]]).
wl:arglist_info(u_environmental_object_input, f_u_environmental_object_input, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_environmental_object_input).

/*

### Compiled:  `U::ENVIRONMENTAL-OBJECT-INPUT` 
*/
f_u_environmental_object_input(LetResult13) :-
	nop(global_env(Env)),
	GEnv=Env,
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Taking optional object or concept input")
			  ],
			  Roman_nl_Ret),
	get_var(GEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(GEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_enter_concepts(Xx_reality_xx_Get,
			   Xx_me_belief_path_xx_Get,
			   Concepts_Init),
	LEnv=[bv(u_concepts, Concepts_Init)|GEnv],
	f_u_null_c63(u_concepts, IFTEST),
	(   IFTEST\==[]
	->  LetResult13=[]
	;   get_var(LEnv, u_concepts, Concepts_Get),
	    f_u_run_object_serendipities(Concepts_Get, Result1_Init),
	    f_u_entered_concept_serendipity(Result2_Init),
	    LEnv14=[bv(u_result1, Result1_Init), bv(u_result2, Result2_Init)|LEnv],
	    (   get_var(LEnv14, u_result1, Result1_Get),
		Result1_Get\==[],
		LetResult13=Result1_Get
	    ->  true
	    ;   get_var(LEnv14, u_result2, Result2_Get),
		LetResult13=Result2_Get
	    )
	).
:- set_opv(f_u_environmental_object_input, classof, claz_function),
   set_opv(u_environmental_object_input, compile_as, kw_function),
   set_opv(u_environmental_object_input,
	   function,
	   f_u_environmental_object_input),
   DefunResult=u_environmental_object_input.
/*
:- side_effect(assert_lsp(u_environmental_object_input,
			  wl:lambda_def(defun, u_environmental_object_input, f_u_environmental_object_input, [], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Taking optional object or concept input")], [let, [[u_concepts, [u_enter_concepts, u_xx_reality_xx, u_xx_me_belief_path_xx]]], [if, [u_null_c63, u_concepts], [], [let, [[u_result1, [u_run_object_serendipities, u_concepts]], [u_result2, [u_entered_concept_serendipity]]], [or, u_result1, u_result2]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_environmental_object_input,
			  wl:arglist_info(u_environmental_object_input, f_u_environmental_object_input, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_environmental_object_input,
			  wl:init_args(exact_only, f_u_environmental_object_input))).
*/
/*
(defun run-object-serendipities (concepts)
  (let ((episodes (epmem-retrieve1 (append! concepts *recent-indices*)
                                   t 'reminding-threshold))
        (old-recent-episodes *recent-episodes*)
        (temp nil))
       (setq *recent-episodes* (append! *recent-episodes* episodes))
       (setq temp (run-serendipities))
       (setq *recent-episodes* old-recent-episodes)
       (if temp (yloop (yfor episode in episodes) ; was (cdr temp)
                       (ydo
                        (if (any? (lambda (d) (memq? d (cdr temp)))
                                  (ob$get episode 'descendants))
                            (epmem-reminding episode t nil)))))
       temp))

;
; Episode recency mechanism
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'run-object-serendipities',[concepts],[let,[[episodes,['epmem-retrieve1',['append!',concepts,'*recent-indices*'],t,[quote,'reminding-threshold']]],['old-recent-episodes','*recent-episodes*'],[temp,[]]],[setq,'*recent-episodes*',['append!','*recent-episodes*',episodes]],[setq,temp,['run-serendipities']],[setq,'*recent-episodes*','old-recent-episodes'],[if,temp,[yloop,[yfor,episode,in,episodes],[ydo,[if,['any?',[lambda,[d],['memq?',d,[cdr,temp]]],['ob$get',episode,[quote,descendants]]],['epmem-reminding',episode,t,[]]]]]],temp]])
wl:lambda_def(defun, u_run_object_serendipities, f_u_run_object_serendipities, [u_concepts], [[let, [[u_episodes, [u_epmem_retrieve1, [u_append_c33, u_concepts, u_xx_recent_indices_xx], t, [quote, u_reminding_threshold]]], [u_old_recent_episodes, u_xx_recent_episodes_xx], [u_temp, []]], [setq, u_xx_recent_episodes_xx, [u_append_c33, u_xx_recent_episodes_xx, u_episodes]], [setq, u_temp, [u_run_serendipities]], [setq, u_xx_recent_episodes_xx, u_old_recent_episodes], [if, u_temp, [u_yloop, [u_yfor, u_episode, u_in, u_episodes], [u_ydo, [if, [u_any_c63, [lambda, [u_d], [u_memq_c63, u_d, [cdr, u_temp]]], [u_ob_c36_get, u_episode, [quote, u_descendants]]], [u_epmem_reminding, u_episode, t, []]]]]], u_temp]]).
wl:arglist_info(u_run_object_serendipities, f_u_run_object_serendipities, [u_concepts], arginfo{all:[u_concepts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_concepts], opt:0, req:[u_concepts], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_run_object_serendipities).

/*

### Compiled:  `U::RUN-OBJECT-SERENDIPITIES` 
*/
f_u_run_object_serendipities(Concepts, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_concepts, Concepts)|Env],
	f_u_append_c33(u_concepts, u_xx_recent_indices_xx, Xx_recent_indices_xx),
	f_u_epmem_retrieve1(Xx_recent_indices_xx,
			    t,
			    u_reminding_threshold,
			    Episodes_Init),
	get_var(Env19, u_xx_recent_episodes_xx, Xx_recent_episodes_xx_Get),
	LEnv=[bv(u_episodes, Episodes_Init), bv(u_old_recent_episodes, Xx_recent_episodes_xx_Get), bv(u_temp, [])|Env19],
	f_u_append_c33(u_xx_recent_episodes_xx, u_episodes, Episodes),
	set_var(LEnv, u_xx_recent_episodes_xx, Episodes),
	f_u_run_serendipities(Temp),
	set_var(LEnv, u_temp, Temp),
	get_var(LEnv, u_old_recent_episodes, Old_recent_episodes_Get),
	set_var(LEnv, u_xx_recent_episodes_xx, Old_recent_episodes_Get),
	get_var(LEnv, u_temp, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, u_episode, u_in, u_episodes],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_any_c63,
			      [lambda, [u_d], [u_memq_c63, u_d, [cdr, u_temp]]],
			      [u_ob_c36_get, u_episode, [quote, u_descendants]]
			    ],
			    [u_epmem_reminding, u_episode, t, []]
			  ]
			]
		      ],
		      TrueResult),
	    _232639906=TrueResult
	;   _232639906=[]
	),
	get_var(LEnv, u_temp, Temp_Get16),
	Temp_Get16=FnResult.
:- set_opv(f_u_run_object_serendipities, classof, claz_function),
   set_opv(u_run_object_serendipities, compile_as, kw_function),
   set_opv(u_run_object_serendipities, function, f_u_run_object_serendipities),
   DefunResult=u_run_object_serendipities.
/*
:- side_effect(assert_lsp(u_run_object_serendipities,
			  wl:lambda_def(defun, u_run_object_serendipities, f_u_run_object_serendipities, [u_concepts], [[let, [[u_episodes, [u_epmem_retrieve1, [u_append_c33, u_concepts, u_xx_recent_indices_xx], t, [quote, u_reminding_threshold]]], [u_old_recent_episodes, u_xx_recent_episodes_xx], [u_temp, []]], [setq, u_xx_recent_episodes_xx, [u_append_c33, u_xx_recent_episodes_xx, u_episodes]], [setq, u_temp, [u_run_serendipities]], [setq, u_xx_recent_episodes_xx, u_old_recent_episodes], [if, u_temp, [u_yloop, [u_yfor, u_episode, u_in, u_episodes], [u_ydo, [if, [u_any_c63, [lambda, [u_d], [u_memq_c63, u_d, [cdr, u_temp]]], [u_ob_c36_get, u_episode, [quote, u_descendants]]], [u_epmem_reminding, u_episode, t, []]]]]], u_temp]]))).
*/
/*
:- side_effect(assert_lsp(u_run_object_serendipities,
			  wl:arglist_info(u_run_object_serendipities, f_u_run_object_serendipities, [u_concepts], arginfo{all:[u_concepts], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_concepts], opt:0, req:[u_concepts], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_run_object_serendipities,
			  wl:init_args(exact_only, f_u_run_object_serendipities))).
*/
/*
 was (cdr temp)
*/
/*
*/
/*
 Episode recency mechanism
*/
/*
*/
/*
(setq *recent-episodes* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10191 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*recent-episodes*',[]])
:- set_var(AEnv, setq, u_xx_recent_episodes_xx, []).
/*
(setq *recent-ep-max-length* 4)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10221 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*recent-ep-max-length*',4])
:- set_var(AEnv, setq, u_xx_recent_ep_max_length_xx, 4).
/*
(defun add-recent (episode)
  (yloop (yfor ep in *recent-episodes*)
         (ydo (if (memq? ep (ob$get episode 'descendants))
                  (setq *recent-episodes* (delq! ep *recent-episodes*)))))
  (if (>= (length *recent-episodes*) *recent-ep-max-length*)
      (progn
       (ndbg-roman-nl *gate-dbg* rule "Episode "(defun add-recent (episode)\n  (yloop (yfor ep in *recent-episodes*)\n         (ydo (if (memq? ep (ob$get episode 'descendants))\n                  (setq *recent-episodes* (delq! ep *recent-episodes*)))))\n  (if (>= (length *recent-episodes*) *recent-ep-max-length*)\n      (progn\n       (ndbg-roman-nl *gate-dbg* rule \"Episode ~A fades\"\n                      (car *recent-episodes*))\n       (setq *recent-episodes* (cdr *recent-episodes*))))\n  (setq *recent-episodes* (append! *recent-episodes* (list episode))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10254 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-recent',[episode],[yloop,[yfor,ep,in,'*recent-episodes*'],[ydo,[if,['memq?',ep,['ob$get',episode,[quote,descendants]]],[setq,'*recent-episodes*',['delq!',ep,'*recent-episodes*']]]]],[if,[>=,[length,'*recent-episodes*'],'*recent-ep-max-length*'],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Episode ~A fades"),[car,'*recent-episodes*']],[setq,'*recent-episodes*',[cdr,'*recent-episodes*']]]],[setq,'*recent-episodes*',['append!','*recent-episodes*',[list,episode]]]])
wl:lambda_def(defun, u_add_recent, f_u_add_recent, [u_episode], [[u_yloop, [u_yfor, u_ep, u_in, u_xx_recent_episodes_xx], [u_ydo, [if, [u_memq_c63, u_ep, [u_ob_c36_get, u_episode, [quote, u_descendants]]], [setq, u_xx_recent_episodes_xx, [u_delq_c33, u_ep, u_xx_recent_episodes_xx]]]]], [if, [>=, [length, u_xx_recent_episodes_xx], u_xx_recent_ep_max_length_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Episode ~A fades"), [car, u_xx_recent_episodes_xx]], [setq, u_xx_recent_episodes_xx, [cdr, u_xx_recent_episodes_xx]]]], [setq, u_xx_recent_episodes_xx, [u_append_c33, u_xx_recent_episodes_xx, [list, u_episode]]]]).
wl:arglist_info(u_add_recent, f_u_add_recent, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_add_recent).

/*

### Compiled:  `U::ADD-RECENT` 
*/
f_u_add_recent(Episode, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_episode, Episode)|Env],
	f_u_yloop(
		  [ [u_yfor, u_ep, u_in, u_xx_recent_episodes_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_memq_c63,
			  u_ep,
			  [u_ob_c36_get, u_episode, [quote, u_descendants]]
			],
			
			[ setq,
			  u_xx_recent_episodes_xx,
			  [u_delq_c33, u_ep, u_xx_recent_episodes_xx]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(AEnv, u_xx_recent_episodes_xx, Xx_recent_episodes_xx_Get),
	cl_length(Xx_recent_episodes_xx_Get, PredArg1Result),
	get_var(AEnv,
		u_xx_recent_ep_max_length_xx,
		Xx_recent_ep_max_length_xx_Get),
	(   PredArg1Result>=Xx_recent_ep_max_length_xx_Get
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Episode ~A fades"),
				[car, u_xx_recent_episodes_xx]
			      ],
			      Roman_nl_Ret),
	    get_var(AEnv, u_xx_recent_episodes_xx, Xx_recent_episodes_xx_Get11),
	    cl_cdr(Xx_recent_episodes_xx_Get11, TrueResult),
	    set_var(AEnv, u_xx_recent_episodes_xx, TrueResult),
	    _234445630=TrueResult
	;   _234445630=[]
	),
	f_u_append_c33(u_xx_recent_episodes_xx,
		       [list, u_episode],
		       Xx_recent_episodes_xx),
	set_var(AEnv, u_xx_recent_episodes_xx, Xx_recent_episodes_xx),
	Xx_recent_episodes_xx=FnResult.
:- set_opv(f_u_add_recent, classof, claz_function),
   set_opv(u_add_recent, compile_as, kw_function),
   set_opv(u_add_recent, function, f_u_add_recent),
   DefunResult=u_add_recent.
/*
:- side_effect(assert_lsp(u_add_recent,
			  wl:lambda_def(defun, u_add_recent, f_u_add_recent, [u_episode], [[u_yloop, [u_yfor, u_ep, u_in, u_xx_recent_episodes_xx], [u_ydo, [if, [u_memq_c63, u_ep, [u_ob_c36_get, u_episode, [quote, u_descendants]]], [setq, u_xx_recent_episodes_xx, [u_delq_c33, u_ep, u_xx_recent_episodes_xx]]]]], [if, [>=, [length, u_xx_recent_episodes_xx], u_xx_recent_ep_max_length_xx], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Episode ~A fades"), [car, u_xx_recent_episodes_xx]], [setq, u_xx_recent_episodes_xx, [cdr, u_xx_recent_episodes_xx]]]], [setq, u_xx_recent_episodes_xx, [u_append_c33, u_xx_recent_episodes_xx, [list, u_episode]]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_recent,
			  wl:arglist_info(u_add_recent, f_u_add_recent, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_recent,
			  wl:init_args(exact_only, f_u_add_recent))).
*/
/*
(defun recent-episode? (episode)
  (any? (lambda (ep) (memq? episode (ob$get ep 'descendants)))
        *recent-episodes*))
; was (memq? episode *recent-episodes*)

; Todo: if an episode is defined after the system is already going,
; this should be called.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10764 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'recent-episode?',[episode],['any?',[lambda,[ep],['memq?',episode,['ob$get',ep,[quote,descendants]]]],'*recent-episodes*']])
wl:lambda_def(defun, u_recent_episode_c63, f_u_recent_episode_c63, [u_episode], [[u_any_c63, [lambda, [u_ep], [u_memq_c63, u_episode, [u_ob_c36_get, u_ep, [quote, u_descendants]]]], u_xx_recent_episodes_xx]]).
wl:arglist_info(u_recent_episode_c63, f_u_recent_episode_c63, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_recent_episode_c63).

/*

### Compiled:  `U::RECENT-EPISODE?` 
*/
f_u_recent_episode_c63(Episode, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_episode, Episode)|Env],
	f_u_any_c63(
		    [ lambda,
		      [u_ep],
		      
		      [ u_memq_c63,
			u_episode,
			[u_ob_c36_get, u_ep, [quote, u_descendants]]
		      ]
		    ],
		    u_xx_recent_episodes_xx,
		    Xx_recent_episodes_xx),
	Xx_recent_episodes_xx=FnResult.
:- set_opv(f_u_recent_episode_c63, classof, claz_function),
   set_opv(u_recent_episode_c63, compile_as, kw_function),
   set_opv(u_recent_episode_c63, function, f_u_recent_episode_c63),
   DefunResult=u_recent_episode_c63.
/*
:- side_effect(assert_lsp(u_recent_episode_c63,
			  wl:lambda_def(defun, u_recent_episode_c63, f_u_recent_episode_c63, [u_episode], [[u_any_c63, [lambda, [u_ep], [u_memq_c63, u_episode, [u_ob_c36_get, u_ep, [quote, u_descendants]]]], u_xx_recent_episodes_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_recent_episode_c63,
			  wl:arglist_info(u_recent_episode_c63, f_u_recent_episode_c63, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_recent_episode_c63,
			  wl:init_args(exact_only, f_u_recent_episode_c63))).
*/
/*
 was (memq? episode *recent-episodes*)
*/
/*
 Todo: if an episode is defined after the system is already going,
*/
/*
 this should be called.
*/
/*
(defun epmem-reminding (episode no-serendipities? new-stored-ep?)
  (if (not (recent-episode? episode))
      (progn
  ;
  ; Print out stuff
  ;
  (if (not new-stored-ep?)
      (progn
       (ndbg-roman *gate-dbg* rule "Episodic reminding")
       (ndbg-roman *gate-dbg* rule " of "(defun epmem-reminding (episode no-serendipities? new-stored-ep?)\n  (if (not (recent-episode? episode))\n      (progn\n  ;\n  ; Print out stuff\n  ;\n  (if (not new-stored-ep?)\n      (progn\n       (ndbg-roman *gate-dbg* rule \"Episodic reminding\")\n       (ndbg-roman *gate-dbg* rule \" of ~A\" episode)\n       (ndbg-newline *gate-dbg* rule)\n       (generate-episode episode)))\n  ;\n  ; Add to recent episodes.\n  ;\n  (add-recent episode)\n  ;\n  ; Add other indices of episode to recent indices.\n  ; Note: this effectively results in a `reminding' link (subject\n  ;   to threshold requirements) from any two episodes having\n  ;   the same index.\n  ;\n  (yloop (yfor index in (ob$gets episode 'indexed-under))\n         (ydo \n          (if (ty$instance? index 'emotion)\n              (if (not new-stored-ep?)\n                  (progn\n                   (setq *reality* *reality-lookahead*)\n                   (ndbg-roman-nl *gate-dbg* rule \"Reactivate emotion\")\n                   (add-emotion (ob$get episode 'goal)\n                                index (ob$get episode 'realism) *reality*)))\n              (add-recent-index index))))\n  ;\n  ; Run serendipities unless told not to.\n  ; Note: inacc. planning rules can be plans OR inferences!\n  ;\n  (if (null? no-serendipities?)\n      (run-serendipity (inaccessible-planning-rules episode) nil))\n  ;\n  ; Get any new remindings from indices now active.\n  ;\n  (remindings))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'epmem-reminding',[episode,'no-serendipities?','new-stored-ep?'],[if,[not,['recent-episode?',episode]],[progn,[if,[not,'new-stored-ep?'],[progn,['ndbg-roman','*gate-dbg*',rule,'$STRING'("Episodic reminding")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" of ~A"),episode],['ndbg-newline','*gate-dbg*',rule],['generate-episode',episode]]],['add-recent',episode],[yloop,[yfor,index,in,['ob$gets',episode,[quote,'indexed-under']]],[ydo,[if,['ty$instance?',index,[quote,emotion]],[if,[not,'new-stored-ep?'],[progn,[setq,'*reality*','*reality-lookahead*'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Reactivate emotion")],['add-emotion',['ob$get',episode,[quote,goal]],index,['ob$get',episode,[quote,realism]],'*reality*']]],['add-recent-index',index]]]],[if,['null?','no-serendipities?'],['run-serendipity',['inaccessible-planning-rules',episode],[]]],[remindings]]]])
wl:lambda_def(defun, u_epmem_reminding, f_u_epmem_reminding, [u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], [[if, [not, [u_recent_episode_c63, u_episode]], [progn, [if, [not, u_new_stored_ep_c63], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Episodic reminding")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " of ~A"), u_episode], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_generate_episode, u_episode]]], [u_add_recent, u_episode], [u_yloop, [u_yfor, index, u_in, [u_ob_c36_gets, u_episode, [quote, u_indexed_under]]], [u_ydo, [if, [u_ty_c36_instance_c63, index, [quote, u_emotion]], [if, [not, u_new_stored_ep_c63], [progn, [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Reactivate emotion")], [u_add_emotion, [u_ob_c36_get, u_episode, [quote, u_goal]], index, [u_ob_c36_get, u_episode, [quote, u_realism]], u_xx_reality_xx]]], [u_add_recent_index, index]]]], [if, [u_null_c63, u_no_serendipities_c63], [u_run_serendipity, [u_inaccessible_planning_rules, u_episode], []]], [u_remindings]]]]).
wl:arglist_info(u_epmem_reminding, f_u_epmem_reminding, [u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], arginfo{all:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], opt:0, req:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_epmem_reminding).

/*

### Compiled:  `U::EPMEM-REMINDING` 
*/
f_u_epmem_reminding(Episode, No_serendipities_c63, New_stored_ep_c63, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_episode, Episode), bv(u_no_serendipities_c63, No_serendipities_c63), bv(u_new_stored_ep_c63, New_stored_ep_c63)|Env],
	get_var(Env22, u_episode, Episode_Get),
	f_u_recent_episode_c63(Episode_Get, PredArgResult),
	(   PredArgResult==[]
	->  get_var(Env22, u_new_stored_ep_c63, New_stored_ep_c63_Get),
	    (   New_stored_ep_c63_Get==[]
	    ->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
			       u_rule,
			       
			       [ '$ARRAY'([*],
					  claz_base_character,
					  "Episodic reminding")
			       ],
			       Ndbg_roman_Ret),
		f_u_ndbg_roman(u_xx_gate_dbg_xx,
			       u_rule,
			       
			       [ '$ARRAY'([*], claz_base_character, " of ~A"),
				 u_episode
			       ],
			       Ndbg_roman_Ret29),
		f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
		get_var(Env22, u_episode, Episode_Get12),
		f_u_generate_episode(Episode_Get12, TrueResult),
		_236631962=TrueResult
	    ;   _236631962=[]
	    ),
	    get_var(Env22, u_episode, Episode_Get14),
	    f_u_add_recent(Episode_Get14, Add_recent_Ret),
	    f_u_yloop(
		      [ 
			[ u_yfor,
			  index,
			  u_in,
			  [u_ob_c36_gets, u_episode, [quote, u_indexed_under]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_ty_c36_instance_c63, index, [quote, u_emotion]],
			    
			    [ if,
			      [not, u_new_stored_ep_c63],
			      
			      [ progn,
				
				[ setq,
				  u_xx_reality_xx,
				  u_xx_reality_lookahead_xx
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   "Reactivate emotion")
				],
				
				[ u_add_emotion,
				  [u_ob_c36_get, u_episode, [quote, u_goal]],
				  index,
				  [u_ob_c36_get, u_episode, [quote, u_realism]],
				  u_xx_reality_xx
				]
			      ]
			    ],
			    [u_add_recent_index, index]
			  ]
			]
		      ],
		      Yloop_Ret),
	    f_u_null_c63(u_no_serendipities_c63, IFTEST15),
	    (   IFTEST15\==[]
	    ->  get_var(Env22, u_episode, Episode_Get17),
		f_u_inaccessible_planning_rules(Episode_Get17,
						Run_serendipity_Param),
		f_u_run_serendipity(Run_serendipity_Param, [], TrueResult18),
		_236696098=TrueResult18
	    ;   _236696098=[]
	    ),
	    f_u_remindings(TrueResult19),
	    FnResult=TrueResult19
	;   FnResult=[]
	).
:- set_opv(f_u_epmem_reminding, classof, claz_function),
   set_opv(u_epmem_reminding, compile_as, kw_function),
   set_opv(u_epmem_reminding, function, f_u_epmem_reminding),
   DefunResult=u_epmem_reminding.
/*
:- side_effect(assert_lsp(u_epmem_reminding,
			  wl:lambda_def(defun, u_epmem_reminding, f_u_epmem_reminding, [u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], [[if, [not, [u_recent_episode_c63, u_episode]], [progn, [if, [not, u_new_stored_ep_c63], [progn, [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Episodic reminding")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " of ~A"), u_episode], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_generate_episode, u_episode]]], [u_add_recent, u_episode], [u_yloop, [u_yfor, index, u_in, [u_ob_c36_gets, u_episode, [quote, u_indexed_under]]], [u_ydo, [if, [u_ty_c36_instance_c63, index, [quote, u_emotion]], [if, [not, u_new_stored_ep_c63], [progn, [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Reactivate emotion")], [u_add_emotion, [u_ob_c36_get, u_episode, [quote, u_goal]], index, [u_ob_c36_get, u_episode, [quote, u_realism]], u_xx_reality_xx]]], [u_add_recent_index, index]]]], [if, [u_null_c63, u_no_serendipities_c63], [u_run_serendipity, [u_inaccessible_planning_rules, u_episode], []]], [u_remindings]]]]))).
*/
/*
:- side_effect(assert_lsp(u_epmem_reminding,
			  wl:arglist_info(u_epmem_reminding, f_u_epmem_reminding, [u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], arginfo{all:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], opt:0, req:[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_epmem_reminding,
			  wl:init_args(exact_only, f_u_epmem_reminding))).
*/
/*
*/
/*
 Print out stuff
*/
/*
*/
/*
*/
/*
 Add to recent episodes.
*/
/*
*/
/*
*/
/*
 Add other indices of episode to recent indices.
*/
/*
 Note: this effectively results in a `reminding' link (subject
*/
/*
   to threshold requirements) from any two episodes having
*/
/*
   the same index.
*/
/*
*/
/*
*/
/*
 Run serendipities unless told not to.
*/
/*
 Note: inacc. planning rules can be plans OR inferences!
*/
/*
*/
/*
*/
/*
 Get any new remindings from indices now active.
*/
/*
*/
/*
(setq *auto-rule-plausibility* 0.7)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12431 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*auto-rule-plausibility*',0.7])
:- set_var(AEnv, setq, u_xx_auto_rule_plausibility_xx, 0.7).
/*
(defun episode-defn-goal (defn)
  (car defn))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12468 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn-goal',[defn],[car,defn]])
wl:lambda_def(defun, u_episode_defn_goal, f_u_episode_defn_goal, [u_defn], [[car, u_defn]]).
wl:arglist_info(u_episode_defn_goal, f_u_episode_defn_goal, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_goal).

/*

### Compiled:  `U::EPISODE-DEFN-GOAL` 
*/
f_u_episode_defn_goal(Defn, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_defn, Defn)|Env],
	get_var(Env7, u_defn, Defn_Get),
	cl_car(Defn_Get, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_episode_defn_goal, classof, claz_function),
   set_opv(u_episode_defn_goal, compile_as, kw_function),
   set_opv(u_episode_defn_goal, function, f_u_episode_defn_goal),
   DefunResult=u_episode_defn_goal.
/*
:- side_effect(assert_lsp(u_episode_defn_goal,
			  wl:lambda_def(defun, u_episode_defn_goal, f_u_episode_defn_goal, [u_defn], [[car, u_defn]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_goal,
			  wl:arglist_info(u_episode_defn_goal, f_u_episode_defn_goal, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_goal,
			  wl:init_args(exact_only, f_u_episode_defn_goal))).
*/
/*
(defun episode-defn-subgoals? (defn)
  (cdr defn))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12515 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn-subgoals?',[defn],[cdr,defn]])
wl:lambda_def(defun, u_episode_defn_subgoals_c63, f_u_episode_defn_subgoals_c63, [u_defn], [[cdr, u_defn]]).
wl:arglist_info(u_episode_defn_subgoals_c63, f_u_episode_defn_subgoals_c63, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_subgoals_c63).

/*

### Compiled:  `U::EPISODE-DEFN-SUBGOALS?` 
*/
f_u_episode_defn_subgoals_c63(Defn, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_defn, Defn)|Env],
	get_var(Env7, u_defn, Defn_Get),
	cl_cdr(Defn_Get, Cdr_Ret),
	Cdr_Ret=FnResult.
:- set_opv(f_u_episode_defn_subgoals_c63, classof, claz_function),
   set_opv(u_episode_defn_subgoals_c63, compile_as, kw_function),
   set_opv(u_episode_defn_subgoals_c63, function, f_u_episode_defn_subgoals_c63),
   DefunResult=u_episode_defn_subgoals_c63.
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals_c63,
			  wl:lambda_def(defun, u_episode_defn_subgoals_c63, f_u_episode_defn_subgoals_c63, [u_defn], [[cdr, u_defn]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals_c63,
			  wl:arglist_info(u_episode_defn_subgoals_c63, f_u_episode_defn_subgoals_c63, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals_c63,
			  wl:init_args(exact_only, f_u_episode_defn_subgoals_c63))).
*/
/*
(defun episode-defn-rule (defn)
  (cadr defn))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12567 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn-rule',[defn],[cadr,defn]])
wl:lambda_def(defun, u_episode_defn_rule, f_u_episode_defn_rule, [u_defn], [[cadr, u_defn]]).
wl:arglist_info(u_episode_defn_rule, f_u_episode_defn_rule, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_rule).

/*

### Compiled:  `U::EPISODE-DEFN-RULE` 
*/
f_u_episode_defn_rule(Defn, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_defn, Defn)|Env],
	get_var(Env7, u_defn, Defn_Get),
	cl_cadr(Defn_Get, Cadr_Ret),
	Cadr_Ret=FnResult.
:- set_opv(f_u_episode_defn_rule, classof, claz_function),
   set_opv(u_episode_defn_rule, compile_as, kw_function),
   set_opv(u_episode_defn_rule, function, f_u_episode_defn_rule),
   DefunResult=u_episode_defn_rule.
/*
:- side_effect(assert_lsp(u_episode_defn_rule,
			  wl:lambda_def(defun, u_episode_defn_rule, f_u_episode_defn_rule, [u_defn], [[cadr, u_defn]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_rule,
			  wl:arglist_info(u_episode_defn_rule, f_u_episode_defn_rule, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_rule,
			  wl:init_args(exact_only, f_u_episode_defn_rule))).
*/
/*
(defun episode-defn-subgoals (defn)
  (cdddr defn))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12615 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn-subgoals',[defn],[cdddr,defn]])
wl:lambda_def(defun, u_episode_defn_subgoals, f_u_episode_defn_subgoals, [u_defn], [[cdddr, u_defn]]).
wl:arglist_info(u_episode_defn_subgoals, f_u_episode_defn_subgoals, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_subgoals).

/*

### Compiled:  `U::EPISODE-DEFN-SUBGOALS` 
*/
f_u_episode_defn_subgoals(Defn, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_defn, Defn)|Env],
	get_var(Env7, u_defn, Defn_Get),
	cl_cdddr(Defn_Get, Cdddr_Ret),
	Cdddr_Ret=FnResult.
:- set_opv(f_u_episode_defn_subgoals, classof, claz_function),
   set_opv(u_episode_defn_subgoals, compile_as, kw_function),
   set_opv(u_episode_defn_subgoals, function, f_u_episode_defn_subgoals),
   DefunResult=u_episode_defn_subgoals.
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals,
			  wl:lambda_def(defun, u_episode_defn_subgoals, f_u_episode_defn_subgoals, [u_defn], [[cdddr, u_defn]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals,
			  wl:arglist_info(u_episode_defn_subgoals, f_u_episode_defn_subgoals, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_subgoals,
			  wl:init_args(exact_only, f_u_episode_defn_subgoals))).
*/
/*
(defun episode-defn-plan-no-gen (defn)
  (caddr defn))

; If T, non top-level goals of a hand-coded episode are not accessible
; directly for planning.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12668 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn-plan-no-gen',[defn],[caddr,defn]])
wl:lambda_def(defun, u_episode_defn_plan_no_gen, f_u_episode_defn_plan_no_gen, [u_defn], [[caddr, u_defn]]).
wl:arglist_info(u_episode_defn_plan_no_gen, f_u_episode_defn_plan_no_gen, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_plan_no_gen).

/*

### Compiled:  `U::EPISODE-DEFN-PLAN-NO-GEN` 
*/
f_u_episode_defn_plan_no_gen(Defn, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_defn, Defn)|Env],
	get_var(Env7, u_defn, Defn_Get),
	cl_caddr(Defn_Get, Caddr_Ret),
	Caddr_Ret=FnResult.
:- set_opv(f_u_episode_defn_plan_no_gen, classof, claz_function),
   set_opv(u_episode_defn_plan_no_gen, compile_as, kw_function),
   set_opv(u_episode_defn_plan_no_gen, function, f_u_episode_defn_plan_no_gen),
   DefunResult=u_episode_defn_plan_no_gen.
/*
:- side_effect(assert_lsp(u_episode_defn_plan_no_gen,
			  wl:lambda_def(defun, u_episode_defn_plan_no_gen, f_u_episode_defn_plan_no_gen, [u_defn], [[caddr, u_defn]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_plan_no_gen,
			  wl:arglist_info(u_episode_defn_plan_no_gen, f_u_episode_defn_plan_no_gen, [u_defn], arginfo{all:[u_defn], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_defn], opt:0, req:[u_defn], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_plan_no_gen,
			  wl:init_args(exact_only, f_u_episode_defn_plan_no_gen))).
*/
/*
 If T, non top-level goals of a hand-coded episode are not accessible
*/
/*
 directly for planning.
*/
/*
(setq *hidden-ep-subgoals?* t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12820 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*hidden-ep-subgoals?*',t])
:- set_var(AEnv, setq, u_xx_hidden_ep_subgoals_c63_xx, t).
/*
(defun episode-defn->stored-episode (episode-defn context hidden?)
  (let ((goal (ob$create `(SUCCEEDED-GOAL
               obj ,(episode-defn-goal episode-defn))))
        (rule nil) (intend nil) (intends nil) (subgoal nil) (subgoals nil)
        (realism 1.0) (num-subgoals nil) (weight nil)
        (plan-no-gen (episode-defn-plan-no-gen episode-defn)))
       (if (not (episode-defn-subgoals? episode-defn))
           (progn
            (no-gen (cx$assert context goal))
            goal)
           (progn
            (setq rule (episode-defn-rule episode-defn))
            (if (eq? rule 'induce-rule)
                (setq rule nil)
                (progn
                 (setq rule (ob$name->ob rule))
                 (if (null? rule)
                     (error "Rule "(defun episode-defn->stored-episode (episode-defn context hidden?)\n  (let ((goal (ob$create `(SUCCEEDED-GOAL\n               obj ,(episode-defn-goal episode-defn))))\n        (rule nil) (intend nil) (intends nil) (subgoal nil) (subgoals nil)\n        (realism 1.0) (num-subgoals nil) (weight nil)\n        (plan-no-gen (episode-defn-plan-no-gen episode-defn)))\n       (if (not (episode-defn-subgoals? episode-defn))\n           (progn\n            (no-gen (cx$assert context goal))\n            goal)\n           (progn\n            (setq rule (episode-defn-rule episode-defn))\n            (if (eq? rule 'induce-rule)\n                (setq rule nil)\n                (progn\n                 (setq rule (ob$name->ob rule))\n                 (if (null? rule)\n                     (error \"Rule ~A not defined yet; (ret) to induce\"\n                            (episode-defn-rule episode-defn)))))\n            (setq realism 0.0)\n            (setq num-subgoals\n                 (fixnum->flonum (length (episode-defn-subgoals episode-defn))))\n            (setq weight\n                 (if rule\n                     (fl/ (ob$get rule 'plausibility) num-subgoals)\n                     (fl/ *auto-rule-plausibility* num-subgoals)))\n       (yloop (initial (subgoalnum 0))\n             (yfor subgoal-spec in (episode-defn-subgoals episode-defn))\n             (ydo \n              (setq subgoal (episode-defn->stored-episode\n                            subgoal-spec context\n                            (or *hidden-ep-subgoals?* hidden?)))\n              (ob$set (ob$get subgoal 'obj) 'plan-subgoalnum subgoalnum)\n              (setq realism (fl+ realism\n                                (fl* weight (strength (ob$get subgoal 'obj)))))\n              (setq subgoals (append subgoals (list subgoal)))\n              (if rule\n                  (setq intend (ob$fcreate `(INTENDS linked-from ,goal\n                                                     linked-to ,subgoal\n                                                     rule ,rule\n                                                     seq? 't)))\n                  (setq intend (ob$fcreate `(INTENDS linked-from ,goal\n                                                     linked-to ,subgoal\n                                                     seq? 't))))\n              (setq intends (cons intend intends))\n              (no-gen (cx$assert context intend))\n              (setq subgoalnum (+ 1 subgoalnum))))\n       (no-gen (cx$assert context goal))\n       (if (null? rule)\n           (progn\n            (ndbg-roman-nl *gate-dbg* rule \"Generating rule automatically.\")\n            (setq rule (plan->rule (ob$get goal 'obj)\n                                  (subgoal-objs subgoals)\n                                  *auto-rule-plausibility*\n                                  #'*episodic-rule-name-genproc*))\n            (if (not (nil? plan-no-gen))\n                (ob$set rule 'plan-no-gen plan-no-gen))\n            (yloop (yfor i in intends)\n                   (ydo (ob$add i 'rule rule))))\n;           (check-episodic-plan rule (ob$get goal 'obj) subgoals)\n       )\n       (ob$set goal '(obj strength) realism)\n       (make-and-store-episode rule goal context realism nil hidden?\n                               (subgoals->eps subgoals))\n       goal))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12852 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-defn->stored-episode',['episode-defn',context,'hidden?'],[let,[[goal,['ob$create',['#BQ',['SUCCEEDED-GOAL',obj,['#COMMA',['episode-defn-goal','episode-defn']]]]]],[rule,[]],[intend,[]],[intends,[]],[subgoal,[]],[subgoals,[]],[realism,1.0],['num-subgoals',[]],[weight,[]],['plan-no-gen',['episode-defn-plan-no-gen','episode-defn']]],[if,[not,['episode-defn-subgoals?','episode-defn']],[progn,['no-gen',['cx$assert',context,goal]],goal],[progn,[setq,rule,['episode-defn-rule','episode-defn']],[if,['eq?',rule,[quote,'induce-rule']],[setq,rule,[]],[progn,[setq,rule,['ob$name->ob',rule]],[if,['null?',rule],[error,'$STRING'("Rule ~A not defined yet; (ret) to induce"),['episode-defn-rule','episode-defn']]]]],[setq,realism,0.0],[setq,'num-subgoals',['fixnum->flonum',[length,['episode-defn-subgoals','episode-defn']]]],[setq,weight,[if,rule,['fl/',['ob$get',rule,[quote,plausibility]],'num-subgoals'],['fl/','*auto-rule-plausibility*','num-subgoals']]],[yloop,[initial,[subgoalnum,0]],[yfor,'subgoal-spec',in,['episode-defn-subgoals','episode-defn']],[ydo,[setq,subgoal,['episode-defn->stored-episode','subgoal-spec',context,[or,'*hidden-ep-subgoals?*','hidden?']]],['ob$set',['ob$get',subgoal,[quote,obj]],[quote,'plan-subgoalnum'],subgoalnum],[setq,realism,['fl+',realism,['fl*',weight,[strength,['ob$get',subgoal,[quote,obj]]]]]],[setq,subgoals,[append,subgoals,[list,subgoal]]],[if,rule,[setq,intend,['ob$fcreate',['#BQ',['INTENDS','linked-from',['#COMMA',goal],'linked-to',['#COMMA',subgoal],rule,['#COMMA',rule],'seq?',[quote,t]]]]],[setq,intend,['ob$fcreate',['#BQ',['INTENDS','linked-from',['#COMMA',goal],'linked-to',['#COMMA',subgoal],'seq?',[quote,t]]]]]],[setq,intends,[cons,intend,intends]],['no-gen',['cx$assert',context,intend]],[setq,subgoalnum,[+,1,subgoalnum]]]],['no-gen',['cx$assert',context,goal]],[if,['null?',rule],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Generating rule automatically.")],[setq,rule,['plan->rule',['ob$get',goal,[quote,obj]],['subgoal-objs',subgoals],'*auto-rule-plausibility*',function('*episodic-rule-name-genproc*')]],[if,[not,['nil?','plan-no-gen']],['ob$set',rule,[quote,'plan-no-gen'],'plan-no-gen']],[yloop,[yfor,i,in,intends],[ydo,['ob$add',i,[quote,rule],rule]]]]],['ob$set',goal,[quote,[obj,strength]],realism],['make-and-store-episode',rule,goal,context,realism,[],'hidden?',['subgoals->eps',subgoals]],goal]]]])
wl:lambda_def(defun, u_episode_defn_c62_stored_episode, f_u_episode_defn_c62_stored_episode, [u_episode_defn, u_context, u_hidden_c63], [[let, [[u_goal, [u_ob_c36_create, ['#BQ', [u_succeeded_goal, u_obj, ['#COMMA', [u_episode_defn_goal, u_episode_defn]]]]]], [u_rule, []], [u_intend, []], [u_intends, []], [u_subgoal, []], [u_subgoals, []], [u_realism, 1.0], [u_num_subgoals, []], [u_weight, []], [u_plan_no_gen, [u_episode_defn_plan_no_gen, u_episode_defn]]], [if, [not, [u_episode_defn_subgoals_c63, u_episode_defn]], [progn, [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], u_goal], [progn, [setq, u_rule, [u_episode_defn_rule, u_episode_defn]], [if, [u_eq_c63, u_rule, [quote, u_induce_rule]], [setq, u_rule, []], [progn, [setq, u_rule, [u_ob_c36_name_c62_ob, u_rule]], [if, [u_null_c63, u_rule], [error, '$ARRAY'([*], claz_base_character, "Rule ~A not defined yet; (ret) to induce"), [u_episode_defn_rule, u_episode_defn]]]]], [setq, u_realism, 0.0], [setq, u_num_subgoals, [u_fixnum_c62_flonum, [length, [u_episode_defn_subgoals, u_episode_defn]]]], [setq, u_weight, [if, u_rule, [u_fl_c47, [u_ob_c36_get, u_rule, [quote, u_plausibility]], u_num_subgoals], [u_fl_c47, u_xx_auto_rule_plausibility_xx, u_num_subgoals]]], [u_yloop, [u_initial, [u_subgoalnum, 0]], [u_yfor, u_subgoal_spec, u_in, [u_episode_defn_subgoals, u_episode_defn]], [u_ydo, [setq, u_subgoal, [u_episode_defn_c62_stored_episode, u_subgoal_spec, u_context, [or, u_xx_hidden_ep_subgoals_c63_xx, u_hidden_c63]]], [u_ob_c36_set, [u_ob_c36_get, u_subgoal, [quote, u_obj]], [quote, u_plan_subgoalnum], u_subgoalnum], [setq, u_realism, [u_fl_c43, u_realism, [u_fl_xx, u_weight, [u_strength, [u_ob_c36_get, u_subgoal, [quote, u_obj]]]]]], [setq, u_subgoals, [append, u_subgoals, [list, u_subgoal]]], [if, u_rule, [setq, u_intend, [u_ob_c36_fcreate, ['#BQ', [u_intends, u_linked_from, ['#COMMA', u_goal], u_linked_to, ['#COMMA', u_subgoal], u_rule, ['#COMMA', u_rule], u_seq_c63, [quote, t]]]]], [setq, u_intend, [u_ob_c36_fcreate, ['#BQ', [u_intends, u_linked_from, ['#COMMA', u_goal], u_linked_to, ['#COMMA', u_subgoal], u_seq_c63, [quote, t]]]]]], [setq, u_intends, [cons, u_intend, u_intends]], [u_no_gen, [u_cx_c36_assert, u_context, u_intend]], [setq, u_subgoalnum, [+, 1, u_subgoalnum]]]], [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], [if, [u_null_c63, u_rule], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Generating rule automatically.")], [setq, u_rule, [u_plan_c62_rule, [u_ob_c36_get, u_goal, [quote, u_obj]], [u_subgoal_objs, u_subgoals], u_xx_auto_rule_plausibility_xx, function(u_xx_episodic_rule_name_genproc_xx)]], [if, [not, [u_nil_c63, u_plan_no_gen]], [u_ob_c36_set, u_rule, [quote, u_plan_no_gen], u_plan_no_gen]], [u_yloop, [u_yfor, u_i, u_in, u_intends], [u_ydo, [u_ob_c36_add, u_i, [quote, u_rule], u_rule]]]]], [u_ob_c36_set, u_goal, [quote, [u_obj, u_strength]], u_realism], [u_make_and_store_episode, u_rule, u_goal, u_context, u_realism, [], u_hidden_c63, [u_subgoals_c62_eps, u_subgoals]], u_goal]]]]).
wl:arglist_info(u_episode_defn_c62_stored_episode, f_u_episode_defn_c62_stored_episode, [u_episode_defn, u_context, u_hidden_c63], arginfo{all:[u_episode_defn, u_context, u_hidden_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode_defn, u_context, u_hidden_c63], opt:0, req:[u_episode_defn, u_context, u_hidden_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_defn_c62_stored_episode).

/*

### Compiled:  `U::EPISODE-DEFN->STORED-EPISODE` 
*/
f_u_episode_defn_c62_stored_episode(Episode_defn, Context, Hidden_c63, FnResult) :-
	nop(global_env(Env)),
	Env55=[bv(u_episode_defn, Episode_defn), bv(u_context, Context), bv(u_hidden_c63, Hidden_c63)|Env],
	f_u_ob_c36_create(
			  [ '#BQ',
			    
			    [ u_succeeded_goal,
			      u_obj,
			      ['#COMMA', [u_episode_defn_goal, u_episode_defn]]
			    ]
			  ],
			  Goal_Init),
	get_var(Env55, u_episode_defn, Episode_defn_Get),
	f_u_episode_defn_plan_no_gen(Episode_defn_Get, Plan_no_gen_Init),
	LEnv=[bv(u_goal, Goal_Init), bv(u_rule, []), bv(u_intend, []), bv(u_intends, []), bv(u_subgoal, []), bv(u_subgoals, []), bv(u_realism, 1.0), bv(u_num_subgoals, []), bv(u_weight, []), bv(u_plan_no_gen, Plan_no_gen_Init)|Env55],
	get_var(LEnv, u_episode_defn, Episode_defn_Get11),
	f_u_episode_defn_subgoals_c63(Episode_defn_Get11, PredArgResult),
	(   PredArgResult==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], No_gen_Ret),
	    get_var(LEnv, u_goal, Goal_Get),
	    FnResult=Goal_Get
	;   get_var(LEnv, u_episode_defn, Episode_defn_Get16),
	    f_u_episode_defn_rule(Episode_defn_Get16, Rule),
	    set_var(LEnv, u_rule, Rule),
	    f_u_eq_c63(u_rule, [quote, u_induce_rule], IFTEST17),
	    (   IFTEST17\==[]
	    ->  set_var(LEnv, setq, u_rule, []),
		ElseResult=[]
	    ;   get_var(LEnv, u_rule, Rule_Get),
		f_u_ob_c36_name_c62_ob(Rule_Get, Rule60),
		set_var(LEnv, u_rule, Rule60),
		f_u_null_c63(u_rule, IFTEST20),
		(   IFTEST20\==[]
		->  get_var(LEnv, u_episode_defn, Episode_defn_Get22),
		    f_u_episode_defn_rule(Episode_defn_Get22, Defn_rule_Ret),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					"Rule ~A not defined yet; (ret) to induce"),
			       Defn_rule_Ret
			     ],
			     TrueResult),
		    ElseResult=TrueResult
		;   ElseResult=[]
		)
	    ),
	    set_var(LEnv, setq, u_realism, 0.0),
	    f_u_fixnum_c62_flonum(
				  [ length,
				    [u_episode_defn_subgoals, u_episode_defn]
				  ],
				  Num_subgoals),
	    set_var(LEnv, u_num_subgoals, Num_subgoals),
	    get_var(LEnv, u_rule, IFTEST25),
	    (   IFTEST25\==[]
	    ->  f_u_fl_c47([u_ob_c36_get, u_rule, [quote, u_plausibility]],
			   u_num_subgoals,
			   TrueResult28),
		Weight=TrueResult28
	    ;   f_u_fl_c47(u_xx_auto_rule_plausibility_xx,
			   u_num_subgoals,
			   ElseResult29),
		Weight=ElseResult29
	    ),
	    set_var(LEnv, u_weight, Weight),
	    f_u_yloop(
		      [ [u_initial, [u_subgoalnum, 0]],
			
			[ u_yfor,
			  u_subgoal_spec,
			  u_in,
			  [u_episode_defn_subgoals, u_episode_defn]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_subgoal,
			    
			    [ u_episode_defn_c62_stored_episode,
			      u_subgoal_spec,
			      u_context,
			      [or, u_xx_hidden_ep_subgoals_c63_xx, u_hidden_c63]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    [u_ob_c36_get, u_subgoal, [quote, u_obj]],
			    [quote, u_plan_subgoalnum],
			    u_subgoalnum
			  ],
			  
			  [ setq,
			    u_realism,
			    
			    [ u_fl_c43,
			      u_realism,
			      
			      [ u_fl_xx,
				u_weight,
				
				[ u_strength,
				  [u_ob_c36_get, u_subgoal, [quote, u_obj]]
				]
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_subgoals,
			    [append, u_subgoals, [list, u_subgoal]]
			  ],
			  
			  [ if,
			    u_rule,
			    
			    [ setq,
			      u_intend,
			      
			      [ u_ob_c36_fcreate,
				
				[ '#BQ',
				  
				  [ u_intends,
				    u_linked_from,
				    ['#COMMA', u_goal],
				    u_linked_to,
				    ['#COMMA', u_subgoal],
				    u_rule,
				    ['#COMMA', u_rule],
				    u_seq_c63,
				    [quote, t]
				  ]
				]
			      ]
			    ],
			    
			    [ setq,
			      u_intend,
			      
			      [ u_ob_c36_fcreate,
				
				[ '#BQ',
				  
				  [ u_intends,
				    u_linked_from,
				    ['#COMMA', u_goal],
				    u_linked_to,
				    ['#COMMA', u_subgoal],
				    u_seq_c63,
				    [quote, t]
				  ]
				]
			      ]
			    ]
			  ],
			  [setq, u_intends, [cons, u_intend, u_intends]],
			  [u_no_gen, [u_cx_c36_assert, u_context, u_intend]],
			  [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
			]
		      ],
		      Yloop_Ret),
	    f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], No_gen_Ret69),
	    f_u_null_c63(u_rule, IFTEST30),
	    (   IFTEST30\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "Generating rule automatically.")
				  ],
				  Roman_nl_Ret),
		get_var(LEnv, u_goal, Goal_Get32),
		f_u_ob_c36_get(Goal_Get32, u_obj, Obj),
		get_var(LEnv, u_subgoals, Subgoals_Get),
		f_u_subgoal_objs(Subgoals_Get, Subgoal_objs_Ret),
		get_var(LEnv,
			u_xx_auto_rule_plausibility_xx,
			Xx_auto_rule_plausibility_xx_Get),
		find_operator_or_die(
				     [ var_tracker(u_plan_no_gen)=rw{name:u_plan_no_gen, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_subgoals)=rw{name:u_subgoals, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_weight)=rw{name:u_weight, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_num_subgoals)=rw{name:u_num_subgoals, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_realism)=rw{name:u_realism, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_goal)=rw{name:u_goal, p:0, r:2, ret:0, u:0, vars:[Goal_Get32], w:0},
				       var_tracker(u_hidden_c63)=rw{name:u_hidden_c63, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_hidden_ep_subgoals_c63_xx)=rw{name:u_xx_hidden_ep_subgoals_c63_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_auto_rule_plausibility_xx)=rw{name:u_xx_auto_rule_plausibility_xx, p:0, r:1, ret:0, u:0, vars:[CAR], w:0},
				       var_tracker(u_xx_recent_ep_max_length_xx)=rw{name:u_xx_recent_ep_max_length_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_recent_episodes_xx)=rw{name:u_xx_recent_episodes_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_recent_index_max_length_xx)=rw{name:u_xx_recent_index_max_length_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_neg_list_xx)=rw{name:u_xx_neg_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_pos_list_xx)=rw{name:u_xx_pos_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_pos_neg_list_xx)=rw{name:u_xx_pos_neg_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_neg_emot_ptn_xx)=rw{name:u_xx_neg_emot_ptn_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_pos_emot_ptn_xx)=rw{name:u_xx_pos_emot_ptn_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_recent_indices_xx)=rw{name:u_xx_recent_indices_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_epmem_marks_xx)=rw{name:u_xx_epmem_marks_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_episodic_memory_xx)=rw{name:u_xx_episodic_memory_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_episodes_xx)=rw{name:u_xx_episodes_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_next_ep_number_xx)=rw{name:u_xx_next_ep_number_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_infinite_thresh_xx)=rw{name:u_xx_infinite_thresh_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_genable_emot_thresh_xx)=rw{name:u_xx_genable_emot_thresh_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_emotions_xx)=rw{name:u_xx_emotions_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_top_level_goal_xx)=rw{name:u_xx_top_level_goal_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_top_level_goals_xx)=rw{name:u_xx_top_level_goals_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_emotion_gc_threshold_xx)=rw{name:u_xx_emotion_gc_threshold_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_emotion_decay_factor_xx)=rw{name:u_xx_emotion_decay_factor_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_need_decay_factor_xx)=rw{name:u_xx_need_decay_factor_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_initial_facts_xx)=rw{name:u_xx_initial_facts_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_entered_concepts_xx)=rw{name:u_xx_entered_concepts_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_initial_reality_xx)=rw{name:u_xx_initial_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_first_time_initialize_c63_xx)=rw{name:u_xx_first_time_initialize_c63_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_primal_reality_xx)=rw{name:u_xx_primal_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_reality_lookahead_xx)=rw{name:u_xx_reality_lookahead_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_reality_xx)=rw{name:u_xx_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_state_xx)=rw{name:u_xx_state_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_starting_state_xx)=rw{name:u_xx_starting_state_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_bd2)=rw{name:u_bd2, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_bd1)=rw{name:u_bd1, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_threshold)=rw{name:u_threshold, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_candidate)=rw{name:u_candidate, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_concepts)=rw{name:u_concepts, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_pattern)=rw{name:u_pattern, p:1, r:1, ret:0, u:0, vars:[CAR73], w:0},
				       var_tracker(u_offset)=rw{name:u_offset, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_str)=rw{name:u_str, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(type)=rw{name:type, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_goal_obj)=rw{name:u_goal_obj, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_episodes)=rw{name:u_episodes, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_subgoalnum)=rw{name:u_subgoalnum, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_ruleob)=rw{name:u_ruleob, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(sys_name)=rw{name:sys_name, p:1, r:1, ret:0, u:0, vars:[CAR74], w:0},
				       var_tracker(u_self)=rw{name:u_self, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_rule)=rw{name:u_rule, p:2, r:7, ret:0, u:0, vars:[CAR76, IFTEST13, Rule_Get16, Rule_Get17, Rule_Get18, CAR75, IFTEST25, Rule_Get38], w:5},
				       var_tracker(u_episode_defn)=rw{name:u_episode_defn, p:1, r:4, ret:0, u:0, vars:[CAR77, Episode_defn_Get11, Episode_defn_Get16, Episode_defn_Get22], w:1},
				       var_tracker(u_reminding_thresh)=rw{name:u_reminding_thresh, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_plan_thresh)=rw{name:u_plan_thresh, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_indices)=rw{name:u_indices, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_subsets)=rw{name:u_subsets, p:2, r:1, ret:0, u:0, vars:[CAR78], w:2},
				       var_tracker(u_target)=rw{name:u_target, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(ext_source)=rw{name:ext_source, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_ignore_slots)=rw{name:u_ignore_slots, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_value)=rw{name:u_value, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_var)=rw{name:u_var, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_var2)=rw{name:u_var2, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_var1)=rw{name:u_var1, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_ob2)=rw{name:u_ob2, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_ob1)=rw{name:u_ob1, p:2, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_spec)=rw{name:u_spec, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(stream)=rw{name:stream, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(sys_slots)=rw{name:sys_slots, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_val)=rw{name:u_val, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_y)=rw{name:u_y, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_path)=rw{name:u_path, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_routine)=rw{name:u_routine, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_obj)=rw{name:u_obj, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_omit_proc)=rw{name:u_omit_proc, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_abstract)=rw{name:u_abstract, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_substit)=rw{name:u_substit, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_include_slots)=rw{name:u_include_slots, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_omit_slots)=rw{name:u_omit_slots, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_depth)=rw{name:u_depth, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_template)=rw{name:u_template, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_fact)=rw{name:u_fact, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_bd)=rw{name:u_bd, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_context)=rw{name:u_context, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_obs)=rw{name:u_obs, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_ob)=rw{name:u_ob, p:4, r:0, ret:0, u:0, vars:[], w:3},
				       var_tracker(u_context_abbr)=rw{name:u_context_abbr, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_form)=rw{name:u_form, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_dbg_stream)=rw{name:u_dbg_stream, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(rest)=rw{name:rest, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_dbg_var)=rw{name:u_dbg_var, p:1, r:1, ret:0, u:0, vars:[CAR79], w:0},
				       var_tracker(n)=rw{name:n, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_label)=rw{name:u_label, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_subst_clause)=rw{name:u_subst_clause, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_arg_list)=rw{name:u_arg_list, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_where_to_add)=rw{name:u_where_to_add, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_maximum_variable)=rw{name:u_maximum_variable, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_end_stuff)=rw{name:u_end_stuff, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_afters)=rw{name:u_afters, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_nexts)=rw{name:u_nexts, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_middle_stuff)=rw{name:u_middle_stuff, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_dos)=rw{name:u_dos, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_result)=rw{name:u_result, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_end_tests)=rw{name:u_end_tests, p:0, r:2, ret:0, u:0, vars:[End_tests_Get35], w:0},
				       var_tracker(u_front_stuff)=rw{name:u_front_stuff, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_befores)=rw{name:u_befores, p:0, r:1, ret:0, u:0, vars:[], w:0},
				       var_tracker(bindings)=rw{name:bindings, p:2, r:1, ret:0, u:0, vars:[], w:2},
				       var_tracker(u_block_label)=rw{name:u_block_label, p:0, r:2, ret:0, u:0, vars:[Block_label_Get29], w:0},
				       var_tracker(u_init)=rw{name:u_init, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_from)=rw{name:u_from, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_stepping_variable)=rw{name:u_stepping_variable, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_llist)=rw{name:u_llist, p:0, r:3, ret:0, u:0, vars:[Llist_Get39, Llist_Get44], w:0},
				       var_tracker(u_iteration_expression)=rw{name:u_iteration_expression, p:0, r:14, ret:0, u:0, vars:[Iteration_expression_Get19, Iteration_expression_Get21, Iteration_expression_Get22, Iteration_expression_Get23, Iteration_expression_Get25, Iteration_expression_Get26, Iteration_expression_Get30, Iteration_expression_Get31, Iteration_expression_Get33, Iteration_expression_Get33, Iteration_expression_Get33, Iteration_expression_Get37, Iteration_expression_Get37], w:0},
				       var_tracker(u_iteration_variable)=rw{name:u_iteration_variable, p:0, r:6, ret:0, u:0, vars:[Iteration_variable_Get13, Iteration_variable_Get20, Iteration_variable_Get24, Iteration_variable_Get32, Iteration_variable_Get36], w:0},
				       var_tracker(u_what_to_do)=rw{name:u_what_to_do, p:1, r:1, ret:0, u:0, vars:[CAR80], w:0},
				       var_tracker(variable)=rw{name:variable, p:2, r:1, ret:0, u:0, vars:[CAR81], w:1},
				       var_tracker(u_xx_llist_xx)=rw{name:u_xx_llist_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_what_to_do_xx)=rw{name:u_xx_what_to_do_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_stepping_variable_xx)=rw{name:u_xx_stepping_variable_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_format_args)=rw{name:u_format_args, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_format_string)=rw{name:u_format_string, p:1, r:1, ret:0, u:0, vars:[CAR82], w:0},
				       var_tracker(u_expression)=rw{name:u_expression, p:1, r:3, ret:0, u:0, vars:[CAR83, Expression_Get5, Expression_Get6], w:1},
				       var_tracker(u_clauses)=rw{name:u_clauses, p:0, r:1, ret:0, u:0, vars:[CAR84], w:0},
				       var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(list)=rw{name:list, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_datum)=rw{name:u_datum, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(key)=rw{name:key, p:2, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_clause_key)=rw{name:u_clause_key, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_loop_alist_xx)=rw{name:u_xx_loop_alist_xx, p:0, r:1, ret:0, u:0, vars:[CAR85], w:3},
				       var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(symbol)=rw{name:symbol, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(args)=rw{name:args, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_c)=rw{name:u_c, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_b)=rw{name:u_b, p:1, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_a)=rw{name:u_a, p:1, r:0, ret:0, u:0, vars:[], w:1},
				       var_tracker(u_xx_repl_wont_print_xx)=rw{name:u_xx_repl_wont_print_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_else)=rw{name:u_else, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(u_xx_question_mark_atom_xx)=rw{name:u_xx_question_mark_atom_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(ext_xx_args_xx)=rw{name:ext_xx_args_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(ext_xx_markdown_xx)=rw{name:ext_xx_markdown_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(sys_xx_compiler_mode_xx)=rw{name:sys_xx_compiler_mode_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       var_tracker(sys_xx_output_file_pathname_xx)=rw{name:sys_xx_output_file_pathname_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
				       name='GLOBAL',
				       environ=env_1
				     ],
				     LEnv,
				     u_xx_episodic_rule_name_genproc_xx,
				     Xx_episodic_rule_name_genproc_xx),
		f_u_plan_c62_rule(Obj,
				  Subgoal_objs_Ret,
				  Xx_auto_rule_plausibility_xx_Get,
				  Xx_episodic_rule_name_genproc_xx,
				  Rule65),
		set_var(LEnv, u_rule, Rule65),
		f_u_nil_c63(u_plan_no_gen, PredArgResult37),
		(   PredArgResult37==[]
		->  get_var(LEnv, u_plan_no_gen, Plan_no_gen_Get),
		    get_var(LEnv, u_rule, Rule_Get38),
		    f_u_ob_c36_set(Rule_Get38,
				   u_plan_no_gen,
				   Plan_no_gen_Get,
				   TrueResult40),
		    _242867220=TrueResult40
		;   _242867220=[]
		),
		f_u_yloop(
			  [ [u_yfor, u_i, u_in, u_intends],
			    [u_ydo, [u_ob_c36_add, u_i, [quote, u_rule], u_rule]]
			  ],
			  TrueResult41),
		_242844060=TrueResult41
	    ;   _242844060=[]
	    ),
	    get_var(LEnv, u_goal, Goal_Get42),
	    get_var(LEnv, u_realism, Realism_Get),
	    f_u_ob_c36_set(Goal_Get42,
			   [u_obj, u_strength],
			   Realism_Get,
			   C36_set_Ret),
	    get_var(LEnv, u_context, Context_Get),
	    get_var(LEnv, u_goal, Goal_Get45),
	    get_var(LEnv, u_hidden_c63, Hidden_c63_Get),
	    get_var(LEnv, u_realism, Realism_Get47),
	    get_var(LEnv, u_rule, Rule_Get44),
	    get_var(LEnv, u_subgoals, Subgoals_Get49),
	    f_u_subgoals_c62_eps(Subgoals_Get49, C62_eps_Ret),
	    f_u_make_and_store_episode(Rule_Get44,
				       Goal_Get45,
				       Context_Get,
				       Realism_Get47,
				       [],
				       Hidden_c63_Get,
				       C62_eps_Ret,
				       Store_episode_Ret),
	    get_var(LEnv, u_goal, Goal_Get50),
	    FnResult=Goal_Get50
	).
:- set_opv(f_u_episode_defn_c62_stored_episode, classof, claz_function),
   set_opv(u_episode_defn_c62_stored_episode, compile_as, kw_function),
   set_opv(u_episode_defn_c62_stored_episode,
	   function,
	   f_u_episode_defn_c62_stored_episode),
   DefunResult=u_episode_defn_c62_stored_episode.
/*
:- side_effect(assert_lsp(u_episode_defn_c62_stored_episode,
			  wl:lambda_def(defun, u_episode_defn_c62_stored_episode, f_u_episode_defn_c62_stored_episode, [u_episode_defn, u_context, u_hidden_c63], [[let, [[u_goal, [u_ob_c36_create, ['#BQ', [u_succeeded_goal, u_obj, ['#COMMA', [u_episode_defn_goal, u_episode_defn]]]]]], [u_rule, []], [u_intend, []], [u_intends, []], [u_subgoal, []], [u_subgoals, []], [u_realism, 1.0], [u_num_subgoals, []], [u_weight, []], [u_plan_no_gen, [u_episode_defn_plan_no_gen, u_episode_defn]]], [if, [not, [u_episode_defn_subgoals_c63, u_episode_defn]], [progn, [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], u_goal], [progn, [setq, u_rule, [u_episode_defn_rule, u_episode_defn]], [if, [u_eq_c63, u_rule, [quote, u_induce_rule]], [setq, u_rule, []], [progn, [setq, u_rule, [u_ob_c36_name_c62_ob, u_rule]], [if, [u_null_c63, u_rule], [error, '$ARRAY'([*], claz_base_character, "Rule ~A not defined yet; (ret) to induce"), [u_episode_defn_rule, u_episode_defn]]]]], [setq, u_realism, 0.0], [setq, u_num_subgoals, [u_fixnum_c62_flonum, [length, [u_episode_defn_subgoals, u_episode_defn]]]], [setq, u_weight, [if, u_rule, [u_fl_c47, [u_ob_c36_get, u_rule, [quote, u_plausibility]], u_num_subgoals], [u_fl_c47, u_xx_auto_rule_plausibility_xx, u_num_subgoals]]], [u_yloop, [u_initial, [u_subgoalnum, 0]], [u_yfor, u_subgoal_spec, u_in, [u_episode_defn_subgoals, u_episode_defn]], [u_ydo, [setq, u_subgoal, [u_episode_defn_c62_stored_episode, u_subgoal_spec, u_context, [or, u_xx_hidden_ep_subgoals_c63_xx, u_hidden_c63]]], [u_ob_c36_set, [u_ob_c36_get, u_subgoal, [quote, u_obj]], [quote, u_plan_subgoalnum], u_subgoalnum], [setq, u_realism, [u_fl_c43, u_realism, [u_fl_xx, u_weight, [u_strength, [u_ob_c36_get, u_subgoal, [quote, u_obj]]]]]], [setq, u_subgoals, [append, u_subgoals, [list, u_subgoal]]], [if, u_rule, [setq, u_intend, [u_ob_c36_fcreate, ['#BQ', [u_intends, u_linked_from, ['#COMMA', u_goal], u_linked_to, ['#COMMA', u_subgoal], u_rule, ['#COMMA', u_rule], u_seq_c63, [quote, t]]]]], [setq, u_intend, [u_ob_c36_fcreate, ['#BQ', [u_intends, u_linked_from, ['#COMMA', u_goal], u_linked_to, ['#COMMA', u_subgoal], u_seq_c63, [quote, t]]]]]], [setq, u_intends, [cons, u_intend, u_intends]], [u_no_gen, [u_cx_c36_assert, u_context, u_intend]], [setq, u_subgoalnum, [+, 1, u_subgoalnum]]]], [u_no_gen, [u_cx_c36_assert, u_context, u_goal]], [if, [u_null_c63, u_rule], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Generating rule automatically.")], [setq, u_rule, [u_plan_c62_rule, [u_ob_c36_get, u_goal, [quote, u_obj]], [u_subgoal_objs, u_subgoals], u_xx_auto_rule_plausibility_xx, function(u_xx_episodic_rule_name_genproc_xx)]], [if, [not, [u_nil_c63, u_plan_no_gen]], [u_ob_c36_set, u_rule, [quote, u_plan_no_gen], u_plan_no_gen]], [u_yloop, [u_yfor, u_i, u_in, u_intends], [u_ydo, [u_ob_c36_add, u_i, [quote, u_rule], u_rule]]]]], [u_ob_c36_set, u_goal, [quote, [u_obj, u_strength]], u_realism], [u_make_and_store_episode, u_rule, u_goal, u_context, u_realism, [], u_hidden_c63, [u_subgoals_c62_eps, u_subgoals]], u_goal]]]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_c62_stored_episode,
			  wl:arglist_info(u_episode_defn_c62_stored_episode, f_u_episode_defn_c62_stored_episode, [u_episode_defn, u_context, u_hidden_c63], arginfo{all:[u_episode_defn, u_context, u_hidden_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode_defn, u_context, u_hidden_c63], opt:0, req:[u_episode_defn, u_context, u_hidden_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_defn_c62_stored_episode,
			  wl:init_args(exact_only, f_u_episode_defn_c62_stored_episode))).
*/
/*
           (check-episodic-plan rule (ob$get goal 'obj) subgoals)
*/
/*
(defun subgoals->eps (subgoals)
  (yloop (initial (result nil))
         (yfor subgoal in subgoals)
         (ydo (if (ob$get subgoal 'episode)
                  (setq result (append result (list (ob$get subgoal
                                                     'episode))))))
         (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16124 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'subgoals->eps',[subgoals],[yloop,[initial,[result,[]]],[yfor,subgoal,in,subgoals],[ydo,[if,['ob$get',subgoal,[quote,episode]],[setq,result,[append,result,[list,['ob$get',subgoal,[quote,episode]]]]]]],[yresult,result]]])
wl:lambda_def(defun, u_subgoals_c62_eps, f_u_subgoals_c62_eps, [u_subgoals], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_subgoal, u_in, u_subgoals], [u_ydo, [if, [u_ob_c36_get, u_subgoal, [quote, u_episode]], [setq, u_result, [append, u_result, [list, [u_ob_c36_get, u_subgoal, [quote, u_episode]]]]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_subgoals_c62_eps, f_u_subgoals_c62_eps, [u_subgoals], arginfo{all:[u_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subgoals], opt:0, req:[u_subgoals], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_subgoals_c62_eps).

/*

### Compiled:  `U::SUBGOALS->EPS` 
*/
f_u_subgoals_c62_eps(Subgoals, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_subgoals, Subgoals)|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_subgoal, u_in, u_subgoals],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ob_c36_get, u_subgoal, [quote, u_episode]],
			
			[ setq,
			  u_result,
			  
			  [ append,
			    u_result,
			    [list, [u_ob_c36_get, u_subgoal, [quote, u_episode]]]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_subgoals_c62_eps, classof, claz_function),
   set_opv(u_subgoals_c62_eps, compile_as, kw_function),
   set_opv(u_subgoals_c62_eps, function, f_u_subgoals_c62_eps),
   DefunResult=u_subgoals_c62_eps.
/*
:- side_effect(assert_lsp(u_subgoals_c62_eps,
			  wl:lambda_def(defun, u_subgoals_c62_eps, f_u_subgoals_c62_eps, [u_subgoals], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_subgoal, u_in, u_subgoals], [u_ydo, [if, [u_ob_c36_get, u_subgoal, [quote, u_episode]], [setq, u_result, [append, u_result, [list, [u_ob_c36_get, u_subgoal, [quote, u_episode]]]]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_subgoals_c62_eps,
			  wl:arglist_info(u_subgoals_c62_eps, f_u_subgoals_c62_eps, [u_subgoals], arginfo{all:[u_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subgoals], opt:0, req:[u_subgoals], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_subgoals_c62_eps,
			  wl:init_args(exact_only, f_u_subgoals_c62_eps))).
*/
/*
(defun constructed-plan? (rule)
  (ob$get rule 'constructed?))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16433 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'constructed-plan?',[rule],['ob$get',rule,[quote,'constructed?']]])
wl:lambda_def(defun, u_constructed_plan_c63, f_u_constructed_plan_c63, [u_rule], [[u_ob_c36_get, u_rule, [quote, u_constructed_c63]]]).
wl:arglist_info(u_constructed_plan_c63, f_u_constructed_plan_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_constructed_plan_c63).

/*

### Compiled:  `U::CONSTRUCTED-PLAN?` 
*/
f_u_constructed_plan_c63(Rule, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_rule, Rule)|Env],
	get_var(Env7, u_rule, Rule_Get),
	f_u_ob_c36_get(Rule_Get, u_constructed_c63, Constructed_c63),
	Constructed_c63=FnResult.
:- set_opv(f_u_constructed_plan_c63, classof, claz_function),
   set_opv(u_constructed_plan_c63, compile_as, kw_function),
   set_opv(u_constructed_plan_c63, function, f_u_constructed_plan_c63),
   DefunResult=u_constructed_plan_c63.
/*
:- side_effect(assert_lsp(u_constructed_plan_c63,
			  wl:lambda_def(defun, u_constructed_plan_c63, f_u_constructed_plan_c63, [u_rule], [[u_ob_c36_get, u_rule, [quote, u_constructed_c63]]]))).
*/
/*
:- side_effect(assert_lsp(u_constructed_plan_c63,
			  wl:arglist_info(u_constructed_plan_c63, f_u_constructed_plan_c63, [u_rule], arginfo{all:[u_rule], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule], opt:0, req:[u_rule], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_constructed_plan_c63,
			  wl:init_args(exact_only, f_u_constructed_plan_c63))).
*/
/*
(setq *next-erule-number* 1)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16497 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*next-erule-number*',1])
:- set_var(AEnv, setq, u_xx_next_erule_number_xx, 1).
/*
(defun *episodic-rule-name-genproc* ()
  (string->symbol
   (string-append "EPISODIC-RULE."
                  (prog1 (fixnum->string *next-erule-number*)
                          (increment-me *next-erule-number*)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16527 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'*episodic-rule-name-genproc*',[],['string->symbol',['string-append','$STRING'("EPISODIC-RULE."),[prog1,['fixnum->string','*next-erule-number*'],['increment-me','*next-erule-number*']]]]])
wl:lambda_def(defun, u_xx_episodic_rule_name_genproc_xx, f_u_xx_episodic_rule_name_genproc_xx, [], [[u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "EPISODIC-RULE."), [prog1, [u_fixnum_c62_string, u_xx_next_erule_number_xx], [u_increment_me, u_xx_next_erule_number_xx]]]]]).
wl:arglist_info(u_xx_episodic_rule_name_genproc_xx, f_u_xx_episodic_rule_name_genproc_xx, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_xx_episodic_rule_name_genproc_xx).

/*

### Compiled:  `U::*EPISODIC-RULE-NAME-GENPROC*` 
*/
f_u_xx_episodic_rule_name_genproc_xx(FnResult) :-
	nop(global_env(Env)),
	_259264916=Env,
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*],
					 claz_base_character,
					 "EPISODIC-RULE."),
				
				[ prog1,
				  
				  [ u_fixnum_c62_string,
				    u_xx_next_erule_number_xx
				  ],
				  [u_increment_me, u_xx_next_erule_number_xx]
				]
			      ],
			      C62_symbol_Ret),
	C62_symbol_Ret=FnResult.
:- set_opv(f_u_xx_episodic_rule_name_genproc_xx, classof, claz_function),
   set_opv(u_xx_episodic_rule_name_genproc_xx, compile_as, kw_function),
   set_opv(u_xx_episodic_rule_name_genproc_xx,
	   function,
	   f_u_xx_episodic_rule_name_genproc_xx),
   DefunResult=u_xx_episodic_rule_name_genproc_xx.
/*
:- side_effect(assert_lsp(u_xx_episodic_rule_name_genproc_xx,
			  wl:lambda_def(defun, u_xx_episodic_rule_name_genproc_xx, f_u_xx_episodic_rule_name_genproc_xx, [], [[u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "EPISODIC-RULE."), [prog1, [u_fixnum_c62_string, u_xx_next_erule_number_xx], [u_increment_me, u_xx_next_erule_number_xx]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_xx_episodic_rule_name_genproc_xx,
			  wl:arglist_info(u_xx_episodic_rule_name_genproc_xx, f_u_xx_episodic_rule_name_genproc_xx, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_xx_episodic_rule_name_genproc_xx,
			  wl:init_args(exact_only, f_u_xx_episodic_rule_name_genproc_xx))).
*/
/*
(defun plan->rule (goal subgoals plausibility name-gen-proc)
  (let ((concrete-rule nil)
        (abstract-rule nil))
       (if (cdr subgoals)
           (setq concrete-rule
                (ob$create
                 `(RULE subgoal (RSEQ ,@subgoals)
                        goal ,goal
                        plausibility ,plausibility
                        is 'plan-only)))
           (setq concrete-rule
                (ob$create
                 `(RULE subgoal ,(car subgoals)
                        goal ,goal
                        plausibility ,plausibility
                        is 'plan-only))))
       (setq abstract-rule
        (ob$variabilize concrete-rule #'varize-object? nil *link-slots* nil))
       (ob$add-unique-name abstract-rule (funcall name-gen-proc))
       (ob$set abstract-rule 'constructed? t)
       (add-rule-print abstract-rule)
       abstract-rule))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16747 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'plan->rule',[goal,subgoals,plausibility,'name-gen-proc'],[let,[['concrete-rule',[]],['abstract-rule',[]]],[if,[cdr,subgoals],[setq,'concrete-rule',['ob$create',['#BQ',['RULE',subgoal,['RSEQ',['#BQ-COMMA-ELIPSE',subgoals]],goal,['#COMMA',goal],plausibility,['#COMMA',plausibility],is,[quote,'plan-only']]]]],[setq,'concrete-rule',['ob$create',['#BQ',['RULE',subgoal,['#COMMA',[car,subgoals]],goal,['#COMMA',goal],plausibility,['#COMMA',plausibility],is,[quote,'plan-only']]]]]],[setq,'abstract-rule',['ob$variabilize','concrete-rule',function('varize-object?'),[],'*link-slots*',[]]],['ob$add-unique-name','abstract-rule',[funcall,'name-gen-proc']],['ob$set','abstract-rule',[quote,'constructed?'],t],['add-rule-print','abstract-rule'],'abstract-rule']])
wl:lambda_def(defun, u_plan_c62_rule, f_u_plan_c62_rule, [u_goal, u_subgoals, u_plausibility, u_name_gen_proc], [[let, [[u_concrete_rule, []], [u_abstract_rule, []]], [if, [cdr, u_subgoals], [setq, u_concrete_rule, [u_ob_c36_create, ['#BQ', [u_rule, u_subgoal, [u_rseq, ['#BQ-COMMA-ELIPSE', u_subgoals]], u_goal, ['#COMMA', u_goal], u_plausibility, ['#COMMA', u_plausibility], u_is, [quote, u_plan_only]]]]], [setq, u_concrete_rule, [u_ob_c36_create, ['#BQ', [u_rule, u_subgoal, ['#COMMA', [car, u_subgoals]], u_goal, ['#COMMA', u_goal], u_plausibility, ['#COMMA', u_plausibility], u_is, [quote, u_plan_only]]]]]], [setq, u_abstract_rule, [u_ob_c36_variabilize, u_concrete_rule, function(u_varize_object_c63), [], u_xx_link_slots_xx, []]], [u_ob_c36_add_unique_name, u_abstract_rule, [funcall, u_name_gen_proc]], [u_ob_c36_set, u_abstract_rule, [quote, u_constructed_c63], t], [u_add_rule_print, u_abstract_rule], u_abstract_rule]]).
wl:arglist_info(u_plan_c62_rule, f_u_plan_c62_rule, [u_goal, u_subgoals, u_plausibility, u_name_gen_proc], arginfo{all:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], opt:0, req:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_plan_c62_rule).

/*

### Compiled:  `U::PLAN->RULE` 
*/
f_u_plan_c62_rule(Goal, Subgoals, Plausibility, Name_gen_proc, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_goal, Goal), bv(u_subgoals, Subgoals), bv(u_plausibility, Plausibility), bv(u_name_gen_proc, Name_gen_proc)|Env],
	LEnv=[bv(u_concrete_rule, []), bv(u_abstract_rule, [])|Env22],
	get_var(LEnv, u_subgoals, Subgoals_Get),
	cl_cdr(Subgoals_Get, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_create(
			      [ '#BQ',
				
				[ u_rule,
				  u_subgoal,
				  [u_rseq, ['#BQ-COMMA-ELIPSE', u_subgoals]],
				  u_goal,
				  ['#COMMA', u_goal],
				  u_plausibility,
				  ['#COMMA', u_plausibility],
				  u_is,
				  [quote, u_plan_only]
				]
			      ],
			      TrueResult),
	    set_var(LEnv, u_concrete_rule, TrueResult),
	    _260060302=TrueResult
	;   f_u_ob_c36_create(
			      [ '#BQ',
				
				[ u_rule,
				  u_subgoal,
				  ['#COMMA', [car, u_subgoals]],
				  u_goal,
				  ['#COMMA', u_goal],
				  u_plausibility,
				  ['#COMMA', u_plausibility],
				  u_is,
				  [quote, u_plan_only]
				]
			      ],
			      ElseResult),
	    set_var(LEnv, u_concrete_rule, ElseResult),
	    _260060302=ElseResult
	),
	get_var(LEnv, u_concrete_rule, Concrete_rule_Get),
	find_operator_or_die(
			     [ var_tracker(u_xx_link_slots_xx)=rw{name:u_xx_link_slots_xx, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_abstract_rule)=rw{name:u_abstract_rule, p:0, r:4, ret:0, u:0, vars:[Abstract_rule_Get, Abstract_rule_Get17, Abstract_rule_Get18, Abstract_rule_Get19], w:0},
			       var_tracker(u_concrete_rule)=rw{name:u_concrete_rule, p:0, r:1, ret:0, u:0, vars:[CAR], w:1},
			       var_tracker(u_name_gen_proc)=rw{name:u_name_gen_proc, p:1, r:1, ret:0, u:0, vars:[Name_gen_proc_Get], w:0},
			       var_tracker(u_plausibility)=rw{name:u_plausibility, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_subgoals)=rw{name:u_subgoals, p:1, r:1, ret:0, u:0, vars:[CAR31], w:0},
			       var_tracker(u_goal)=rw{name:u_goal, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_next_erule_number_xx)=rw{name:u_xx_next_erule_number_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_hidden_ep_subgoals_c63_xx)=rw{name:u_xx_hidden_ep_subgoals_c63_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_auto_rule_plausibility_xx)=rw{name:u_xx_auto_rule_plausibility_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_recent_ep_max_length_xx)=rw{name:u_xx_recent_ep_max_length_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_recent_episodes_xx)=rw{name:u_xx_recent_episodes_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_recent_index_max_length_xx)=rw{name:u_xx_recent_index_max_length_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_neg_list_xx)=rw{name:u_xx_neg_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_pos_list_xx)=rw{name:u_xx_pos_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_pos_neg_list_xx)=rw{name:u_xx_pos_neg_list_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_neg_emot_ptn_xx)=rw{name:u_xx_neg_emot_ptn_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_pos_emot_ptn_xx)=rw{name:u_xx_pos_emot_ptn_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_recent_indices_xx)=rw{name:u_xx_recent_indices_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_epmem_marks_xx)=rw{name:u_xx_epmem_marks_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_episodic_memory_xx)=rw{name:u_xx_episodic_memory_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_episodes_xx)=rw{name:u_xx_episodes_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_next_ep_number_xx)=rw{name:u_xx_next_ep_number_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_infinite_thresh_xx)=rw{name:u_xx_infinite_thresh_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_genable_emot_thresh_xx)=rw{name:u_xx_genable_emot_thresh_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_emotions_xx)=rw{name:u_xx_emotions_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_top_level_goal_xx)=rw{name:u_xx_top_level_goal_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_top_level_goals_xx)=rw{name:u_xx_top_level_goals_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_emotion_gc_threshold_xx)=rw{name:u_xx_emotion_gc_threshold_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_emotion_decay_factor_xx)=rw{name:u_xx_emotion_decay_factor_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_need_decay_factor_xx)=rw{name:u_xx_need_decay_factor_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_initial_facts_xx)=rw{name:u_xx_initial_facts_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_entered_concepts_xx)=rw{name:u_xx_entered_concepts_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_initial_reality_xx)=rw{name:u_xx_initial_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_first_time_initialize_c63_xx)=rw{name:u_xx_first_time_initialize_c63_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_primal_reality_xx)=rw{name:u_xx_primal_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_reality_lookahead_xx)=rw{name:u_xx_reality_lookahead_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_reality_xx)=rw{name:u_xx_reality_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_state_xx)=rw{name:u_xx_state_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_starting_state_xx)=rw{name:u_xx_starting_state_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_bd2)=rw{name:u_bd2, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_bd1)=rw{name:u_bd1, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_threshold)=rw{name:u_threshold, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_candidate)=rw{name:u_candidate, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_concepts)=rw{name:u_concepts, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_pattern)=rw{name:u_pattern, p:1, r:1, ret:0, u:0, vars:[CAR32], w:0},
			       var_tracker(u_offset)=rw{name:u_offset, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_str)=rw{name:u_str, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(type)=rw{name:type, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_goal_obj)=rw{name:u_goal_obj, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_episodes)=rw{name:u_episodes, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_subgoalnum)=rw{name:u_subgoalnum, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_ruleob)=rw{name:u_ruleob, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(sys_name)=rw{name:sys_name, p:1, r:1, ret:0, u:0, vars:[CAR33], w:0},
			       var_tracker(u_self)=rw{name:u_self, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_rule)=rw{name:u_rule, p:2, r:4, ret:0, u:0, vars:[CAR34, IFTEST13, Rule_Get16, Rule_Get17, Rule_Get18], w:1},
			       var_tracker(u_episode_defn)=rw{name:u_episode_defn, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_reminding_thresh)=rw{name:u_reminding_thresh, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_plan_thresh)=rw{name:u_plan_thresh, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_indices)=rw{name:u_indices, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_subsets)=rw{name:u_subsets, p:2, r:1, ret:0, u:0, vars:[CAR35], w:2},
			       var_tracker(u_target)=rw{name:u_target, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(ext_source)=rw{name:ext_source, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_ignore_slots)=rw{name:u_ignore_slots, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_value)=rw{name:u_value, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_var)=rw{name:u_var, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_var2)=rw{name:u_var2, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_var1)=rw{name:u_var1, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_ob2)=rw{name:u_ob2, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_ob1)=rw{name:u_ob1, p:2, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_spec)=rw{name:u_spec, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(stream)=rw{name:stream, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(sys_slots)=rw{name:sys_slots, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_val)=rw{name:u_val, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_y)=rw{name:u_y, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_path)=rw{name:u_path, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_routine)=rw{name:u_routine, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_obj)=rw{name:u_obj, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_omit_proc)=rw{name:u_omit_proc, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_abstract)=rw{name:u_abstract, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_substit)=rw{name:u_substit, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_include_slots)=rw{name:u_include_slots, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_omit_slots)=rw{name:u_omit_slots, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_depth)=rw{name:u_depth, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_template)=rw{name:u_template, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_fact)=rw{name:u_fact, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_bd)=rw{name:u_bd, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_context)=rw{name:u_context, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_obs)=rw{name:u_obs, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_ob)=rw{name:u_ob, p:4, r:0, ret:0, u:0, vars:[], w:3},
			       var_tracker(u_context_abbr)=rw{name:u_context_abbr, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_form)=rw{name:u_form, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_dbg_stream)=rw{name:u_dbg_stream, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(rest)=rw{name:rest, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_dbg_var)=rw{name:u_dbg_var, p:1, r:1, ret:0, u:0, vars:[CAR36], w:0},
			       var_tracker(n)=rw{name:n, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_label)=rw{name:u_label, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_subst_clause)=rw{name:u_subst_clause, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_arg_list)=rw{name:u_arg_list, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_where_to_add)=rw{name:u_where_to_add, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_maximum_variable)=rw{name:u_maximum_variable, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_end_stuff)=rw{name:u_end_stuff, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_afters)=rw{name:u_afters, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_nexts)=rw{name:u_nexts, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_middle_stuff)=rw{name:u_middle_stuff, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_dos)=rw{name:u_dos, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_result)=rw{name:u_result, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_end_tests)=rw{name:u_end_tests, p:0, r:2, ret:0, u:0, vars:[End_tests_Get35], w:0},
			       var_tracker(u_front_stuff)=rw{name:u_front_stuff, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_befores)=rw{name:u_befores, p:0, r:1, ret:0, u:0, vars:[], w:0},
			       var_tracker(bindings)=rw{name:bindings, p:2, r:1, ret:0, u:0, vars:[], w:2},
			       var_tracker(u_block_label)=rw{name:u_block_label, p:0, r:2, ret:0, u:0, vars:[Block_label_Get29], w:0},
			       var_tracker(u_init)=rw{name:u_init, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_from)=rw{name:u_from, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_stepping_variable)=rw{name:u_stepping_variable, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_llist)=rw{name:u_llist, p:0, r:3, ret:0, u:0, vars:[Llist_Get39, Llist_Get44], w:0},
			       var_tracker(u_iteration_expression)=rw{name:u_iteration_expression, p:0, r:14, ret:0, u:0, vars:[Iteration_expression_Get19, Iteration_expression_Get21, Iteration_expression_Get22, Iteration_expression_Get23, Iteration_expression_Get25, Iteration_expression_Get26, Iteration_expression_Get30, Iteration_expression_Get31, Iteration_expression_Get33, Iteration_expression_Get33, Iteration_expression_Get33, Iteration_expression_Get37, Iteration_expression_Get37], w:0},
			       var_tracker(u_iteration_variable)=rw{name:u_iteration_variable, p:0, r:6, ret:0, u:0, vars:[Iteration_variable_Get13, Iteration_variable_Get20, Iteration_variable_Get24, Iteration_variable_Get32, Iteration_variable_Get36], w:0},
			       var_tracker(u_what_to_do)=rw{name:u_what_to_do, p:1, r:1, ret:0, u:0, vars:[CAR37], w:0},
			       var_tracker(variable)=rw{name:variable, p:2, r:1, ret:0, u:0, vars:[CAR38], w:1},
			       var_tracker(u_xx_llist_xx)=rw{name:u_xx_llist_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_what_to_do_xx)=rw{name:u_xx_what_to_do_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_stepping_variable_xx)=rw{name:u_xx_stepping_variable_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_format_args)=rw{name:u_format_args, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_format_string)=rw{name:u_format_string, p:1, r:1, ret:0, u:0, vars:[CAR39], w:0},
			       var_tracker(u_expression)=rw{name:u_expression, p:1, r:3, ret:0, u:0, vars:[CAR40, Expression_Get5, Expression_Get6], w:1},
			       var_tracker(u_clauses)=rw{name:u_clauses, p:0, r:1, ret:0, u:0, vars:[CAR41], w:0},
			       var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(list)=rw{name:list, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_datum)=rw{name:u_datum, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(key)=rw{name:key, p:2, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_clause_key)=rw{name:u_clause_key, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_loop_alist_xx)=rw{name:u_xx_loop_alist_xx, p:0, r:1, ret:0, u:0, vars:[CAR42], w:3},
			       var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(symbol)=rw{name:symbol, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(args)=rw{name:args, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_c)=rw{name:u_c, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_b)=rw{name:u_b, p:1, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_a)=rw{name:u_a, p:1, r:0, ret:0, u:0, vars:[], w:1},
			       var_tracker(u_xx_repl_wont_print_xx)=rw{name:u_xx_repl_wont_print_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_else)=rw{name:u_else, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(u_xx_question_mark_atom_xx)=rw{name:u_xx_question_mark_atom_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(ext_xx_args_xx)=rw{name:ext_xx_args_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(ext_xx_markdown_xx)=rw{name:ext_xx_markdown_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(sys_xx_compiler_mode_xx)=rw{name:sys_xx_compiler_mode_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       var_tracker(sys_xx_output_file_pathname_xx)=rw{name:sys_xx_output_file_pathname_xx, p:0, r:0, ret:0, u:0, vars:[], w:0},
			       name='GLOBAL',
			       environ=env_1
			     ],
			     LEnv,
			     u_varize_object_c63,
			     Varize_object_c63),
	get_var(LEnv, u_xx_link_slots_xx, Xx_link_slots_xx_Get),
	f_u_ob_c36_variabilize(Concrete_rule_Get,
			       Varize_object_c63,
			       [],
			       Xx_link_slots_xx_Get,
			       [],
			       Abstract_rule),
	set_var(LEnv, u_abstract_rule, Abstract_rule),
	get_var(LEnv, u_abstract_rule, Abstract_rule_Get),
	get_var(LEnv, u_name_gen_proc, Name_gen_proc_Get),
	cl_apply(Name_gen_proc_Get, [], Apply_Ret),
	f_u_ob_c36_add_unique_name(Abstract_rule_Get,
				   Apply_Ret,
				   Unique_name_Ret),
	get_var(LEnv, u_abstract_rule, Abstract_rule_Get17),
	f_u_ob_c36_set(Abstract_rule_Get17, u_constructed_c63, t, T),
	get_var(LEnv, u_abstract_rule, Abstract_rule_Get18),
	f_u_add_rule_print(Abstract_rule_Get18, Rule_print_Ret),
	get_var(LEnv, u_abstract_rule, Abstract_rule_Get19),
	Abstract_rule_Get19=FnResult.
:- set_opv(f_u_plan_c62_rule, classof, claz_function),
   set_opv(u_plan_c62_rule, compile_as, kw_function),
   set_opv(u_plan_c62_rule, function, f_u_plan_c62_rule),
   DefunResult=u_plan_c62_rule.
/*
:- side_effect(assert_lsp(u_plan_c62_rule,
			  wl:lambda_def(defun, u_plan_c62_rule, f_u_plan_c62_rule, [u_goal, u_subgoals, u_plausibility, u_name_gen_proc], [[let, [[u_concrete_rule, []], [u_abstract_rule, []]], [if, [cdr, u_subgoals], [setq, u_concrete_rule, [u_ob_c36_create, ['#BQ', [u_rule, u_subgoal, [u_rseq, ['#BQ-COMMA-ELIPSE', u_subgoals]], u_goal, ['#COMMA', u_goal], u_plausibility, ['#COMMA', u_plausibility], u_is, [quote, u_plan_only]]]]], [setq, u_concrete_rule, [u_ob_c36_create, ['#BQ', [u_rule, u_subgoal, ['#COMMA', [car, u_subgoals]], u_goal, ['#COMMA', u_goal], u_plausibility, ['#COMMA', u_plausibility], u_is, [quote, u_plan_only]]]]]], [setq, u_abstract_rule, [u_ob_c36_variabilize, u_concrete_rule, function(u_varize_object_c63), [], u_xx_link_slots_xx, []]], [u_ob_c36_add_unique_name, u_abstract_rule, [funcall, u_name_gen_proc]], [u_ob_c36_set, u_abstract_rule, [quote, u_constructed_c63], t], [u_add_rule_print, u_abstract_rule], u_abstract_rule]]))).
*/
/*
:- side_effect(assert_lsp(u_plan_c62_rule,
			  wl:arglist_info(u_plan_c62_rule, f_u_plan_c62_rule, [u_goal, u_subgoals, u_plausibility, u_name_gen_proc], arginfo{all:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], opt:0, req:[u_goal, u_subgoals, u_plausibility, u_name_gen_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_plan_c62_rule,
			  wl:init_args(exact_only, f_u_plan_c62_rule))).
*/
/*
(defun varize-object? (x)
  (and (ob? x)
       (not (vars-in? x))
       (or (and (ty$instance? x 'object)
                (not (ty$instance? x 'no-varize-obj)))
           (ty$instance? x 'city)
           (ty$instance? x 'location))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17639 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'varize-object?',[x],[and,['ob?',x],[not,['vars-in?',x]],[or,[and,['ty$instance?',x,[quote,object]],[not,['ty$instance?',x,[quote,'no-varize-obj']]]],['ty$instance?',x,[quote,city]],['ty$instance?',x,[quote,location]]]]])
wl:lambda_def(defun, u_varize_object_c63, f_u_varize_object_c63, [u_x], [[and, [u_ob_c63, u_x], [not, [u_vars_in_c63, u_x]], [or, [and, [u_ty_c36_instance_c63, u_x, [quote, u_object]], [not, [u_ty_c36_instance_c63, u_x, [quote, u_no_varize_obj]]]], [u_ty_c36_instance_c63, u_x, [quote, u_city]], [u_ty_c36_instance_c63, u_x, [quote, u_location]]]]]).
wl:arglist_info(u_varize_object_c63, f_u_varize_object_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_varize_object_c63).

/*

### Compiled:  `U::VARIZE-OBJECT?` 
*/
f_u_varize_object_c63(X, TrueResult19) :-
	nop(global_env(Env)),
	Env23=[bv(u_x, X)|Env],
	f_u_ob_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env23, u_x, X_Get),
	    f_u_vars_in_c63(X_Get, PredArgResult),
	    (   PredArgResult==[]
	    ->  (   get_var(Env23, u_x, X_Get12),
		    f_u_ty_c36_instance_c63(X_Get12, u_object, IFTEST10),
		    (   IFTEST10\==[]
		    ->  get_var(Env23, u_x, X_Get13),
			f_u_ty_c36_instance_c63(X_Get13,
						u_no_varize_obj,
						No_varize_obj),
			cl_not(No_varize_obj, TrueResult),
			FORM1_Res18=TrueResult
		    ;   FORM1_Res18=[]
		    ),
		    FORM1_Res18\==[],
		    TrueResult19=FORM1_Res18
		->  true
		;   get_var(Env23, u_x, X_Get15),
		    f_u_ty_c36_instance_c63(X_Get15, u_city, FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult19=FORM1_Res
		->  true
		;   get_var(Env23, u_x, X_Get16),
		    f_u_ty_c36_instance_c63(X_Get16, u_location, Location),
		    TrueResult19=Location
		)
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_varize_object_c63, classof, claz_function),
   set_opv(u_varize_object_c63, compile_as, kw_function),
   set_opv(u_varize_object_c63, function, f_u_varize_object_c63),
   DefunResult=u_varize_object_c63.
/*
:- side_effect(assert_lsp(u_varize_object_c63,
			  wl:lambda_def(defun, u_varize_object_c63, f_u_varize_object_c63, [u_x], [[and, [u_ob_c63, u_x], [not, [u_vars_in_c63, u_x]], [or, [and, [u_ty_c36_instance_c63, u_x, [quote, u_object]], [not, [u_ty_c36_instance_c63, u_x, [quote, u_no_varize_obj]]]], [u_ty_c36_instance_c63, u_x, [quote, u_city]], [u_ty_c36_instance_c63, u_x, [quote, u_location]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_varize_object_c63,
			  wl:arglist_info(u_varize_object_c63, f_u_varize_object_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_varize_object_c63,
			  wl:init_args(exact_only, f_u_varize_object_c63))).
*/
/*
(defun subgoal-objs (subgoals)
  (map 'list (lambda (x) (ob$get x 'obj)) subgoals))

; We don't count goals that terminated before the beginning of this
; top-level goal's 'scenario'.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17878 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'subgoal-objs',[subgoals],[map,[quote,list],[lambda,[x],['ob$get',x,[quote,obj]]],subgoals]])
wl:lambda_def(defun, u_subgoal_objs, f_u_subgoal_objs, [u_subgoals], [[map, [quote, list], [lambda, [u_x], [u_ob_c36_get, u_x, [quote, u_obj]]], u_subgoals]]).
wl:arglist_info(u_subgoal_objs, f_u_subgoal_objs, [u_subgoals], arginfo{all:[u_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subgoals], opt:0, req:[u_subgoals], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_subgoal_objs).

/*

### Compiled:  `U::SUBGOAL-OBJS` 
*/
f_u_subgoal_objs(Subgoals, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_subgoals, Subgoals)|Env],
	Lambda=closure([ClosureEnvironment|Env11], LResult, [u_x],  (get_var(ClosureEnvironment, u_x, X_Get), f_u_ob_c36_get(X_Get, u_obj, LResult))),
	get_var(Env11, u_subgoals, Subgoals_Get),
	cl_map(list, Lambda, Subgoals_Get, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_subgoal_objs, classof, claz_function),
   set_opv(u_subgoal_objs, compile_as, kw_function),
   set_opv(u_subgoal_objs, function, f_u_subgoal_objs),
   DefunResult=u_subgoal_objs.
/*
:- side_effect(assert_lsp(u_subgoal_objs,
			  wl:lambda_def(defun, u_subgoal_objs, f_u_subgoal_objs, [u_subgoals], [[map, [quote, list], [lambda, [u_x], [u_ob_c36_get, u_x, [quote, u_obj]]], u_subgoals]]))).
*/
/*
:- side_effect(assert_lsp(u_subgoal_objs,
			  wl:arglist_info(u_subgoal_objs, f_u_subgoal_objs, [u_subgoals], arginfo{all:[u_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_subgoals], opt:0, req:[u_subgoals], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_subgoal_objs,
			  wl:init_args(exact_only, f_u_subgoal_objs))).
*/
/*
 We don't count goals that terminated before the beginning of this
*/
/*
 top-level goal's 'scenario'.
*/
/*
(defun scenario-desirability (context top-level-goal)
  (ndbg-roman-nl *gate-dbg* desire "Assess scenario desirability in "(defun scenario-desirability (context top-level-goal)\n  (ndbg-roman-nl *gate-dbg* desire \"Assess scenario desirability in ~A\" context)\n  (yloop (initial (result 0.0)\n                 (pre-scenario-contexts\n                  (cx$ancestors (ob$get top-level-goal 'activation-context))))\n        (yfor ob in (append (cx$get-all-ty context *failed-goal-ob*)\n                            (cx$get-all-ty context *succeeded-goal-ob*)))\n        (ydo (if (and (personal-goal? ob)\n                     (not (memq? (ob$get ob 'termination-context)\n                                 pre-scenario-contexts)))\n                (progn\n                 (ndbg-roman-nl *gate-dbg* desire \"~A (~A)\"\n                                ob (strength ob))\n                (cond\n                 ((ty$instance? ob 'succeeded-goal)\n                  (setq result (fl+ result (strength ob))))\n                 ((ty$instance? ob 'active-goal)\n                  (setq result (fl- result (strength ob))))\n                 ((ty$instance? ob 'p-goal)\n                  (setq result (fl- result (strength ob))))\n                 ((ty$instance? ob 'failed-goal)\n                  (setq result (fl- result (strength ob))))))))\n        (yresult\n     (progn\n      (ndbg-roman-nl *gate-dbg* rule \"Scenario desirability = ~A\" result)\n      result))))\n\n;\n; This is called upon top-level goal failure or success.\n;\n; Todo: don't store episode if derived from another episode by\n; analogy without repairs.\n;\n; Todo: use situation assumptions as indices?\n;\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'scenario-desirability',[context,'top-level-goal'],['ndbg-roman-nl','*gate-dbg*',desire,'$STRING'("Assess scenario desirability in ~A"),context],[yloop,[initial,[result,0.0],['pre-scenario-contexts',['cx$ancestors',['ob$get','top-level-goal',[quote,'activation-context']]]]],[yfor,ob,in,[append,['cx$get-all-ty',context,'*failed-goal-ob*'],['cx$get-all-ty',context,'*succeeded-goal-ob*']]],[ydo,[if,[and,['personal-goal?',ob],[not,['memq?',['ob$get',ob,[quote,'termination-context']],'pre-scenario-contexts']]],[progn,['ndbg-roman-nl','*gate-dbg*',desire,'$STRING'("~A (~A)"),ob,[strength,ob]],[cond,[['ty$instance?',ob,[quote,'succeeded-goal']],[setq,result,['fl+',result,[strength,ob]]]],[['ty$instance?',ob,[quote,'active-goal']],[setq,result,['fl-',result,[strength,ob]]]],[['ty$instance?',ob,[quote,'p-goal']],[setq,result,['fl-',result,[strength,ob]]]],[['ty$instance?',ob,[quote,'failed-goal']],[setq,result,['fl-',result,[strength,ob]]]]]]]],[yresult,[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Scenario desirability = ~A"),result],result]]]])
wl:lambda_def(defun, u_scenario_desirability, f_u_scenario_desirability, [u_context, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_desire, '$ARRAY'([*], claz_base_character, "Assess scenario desirability in ~A"), u_context], [u_yloop, [u_initial, [u_result, 0.0], [u_pre_scenario_contexts, [u_cx_c36_ancestors, [u_ob_c36_get, u_top_level_goal, [quote, u_activation_context]]]]], [u_yfor, u_ob, u_in, [append, [u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx], [u_cx_c36_get_all_ty, u_context, u_xx_succeeded_goal_ob_xx]]], [u_ydo, [if, [and, [u_personal_goal_c63, u_ob], [not, [u_memq_c63, [u_ob_c36_get, u_ob, [quote, u_termination_context]], u_pre_scenario_contexts]]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_desire, '$ARRAY'([*], claz_base_character, "~A (~A)"), u_ob, [u_strength, u_ob]], [cond, [[u_ty_c36_instance_c63, u_ob, [quote, u_succeeded_goal]], [setq, u_result, [u_fl_c43, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_p_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_failed_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]]]]]], [u_yresult, [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Scenario desirability = ~A"), u_result], u_result]]]]).
wl:arglist_info(u_scenario_desirability, f_u_scenario_desirability, [u_context, u_top_level_goal], arginfo{all:[u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_top_level_goal], opt:0, req:[u_context, u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_scenario_desirability).

/*

### Compiled:  `U::SCENARIO-DESIRABILITY` 
*/
f_u_scenario_desirability(Context, Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_context, Context), bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_desire,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Assess scenario desirability in ~A"),
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, 0.0],
		      
		      [ u_pre_scenario_contexts,
			
			[ u_cx_c36_ancestors,
			  
			  [ u_ob_c36_get,
			    u_top_level_goal,
			    [quote, u_activation_context]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ append,
			[u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx],
			
			[ u_cx_c36_get_all_ty,
			  u_context,
			  u_xx_succeeded_goal_ob_xx
			]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_personal_goal_c63, u_ob],
			  
			  [ not,
			    
			    [ u_memq_c63,
			      
			      [ u_ob_c36_get,
				u_ob,
				[quote, u_termination_context]
			      ],
			      u_pre_scenario_contexts
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_desire,
			    '$ARRAY'([*], claz_base_character, "~A (~A)"),
			    u_ob,
			    [u_strength, u_ob]
			  ],
			  
			  [ cond,
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_succeeded_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_fl_c43, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_active_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ [u_ty_c36_instance_c63, u_ob, [quote, u_p_goal]],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_failed_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   "Scenario desirability = ~A"),
			  u_result
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_scenario_desirability, classof, claz_function),
   set_opv(u_scenario_desirability, compile_as, kw_function),
   set_opv(u_scenario_desirability, function, f_u_scenario_desirability),
   DefunResult=u_scenario_desirability.
/*
:- side_effect(assert_lsp(u_scenario_desirability,
			  wl:lambda_def(defun, u_scenario_desirability, f_u_scenario_desirability, [u_context, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_desire, '$ARRAY'([*], claz_base_character, "Assess scenario desirability in ~A"), u_context], [u_yloop, [u_initial, [u_result, 0.0], [u_pre_scenario_contexts, [u_cx_c36_ancestors, [u_ob_c36_get, u_top_level_goal, [quote, u_activation_context]]]]], [u_yfor, u_ob, u_in, [append, [u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx], [u_cx_c36_get_all_ty, u_context, u_xx_succeeded_goal_ob_xx]]], [u_ydo, [if, [and, [u_personal_goal_c63, u_ob], [not, [u_memq_c63, [u_ob_c36_get, u_ob, [quote, u_termination_context]], u_pre_scenario_contexts]]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_desire, '$ARRAY'([*], claz_base_character, "~A (~A)"), u_ob, [u_strength, u_ob]], [cond, [[u_ty_c36_instance_c63, u_ob, [quote, u_succeeded_goal]], [setq, u_result, [u_fl_c43, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_p_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]], [[u_ty_c36_instance_c63, u_ob, [quote, u_failed_goal]], [setq, u_result, [u_flc45, u_result, [u_strength, u_ob]]]]]]]], [u_yresult, [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Scenario desirability = ~A"), u_result], u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_scenario_desirability,
			  wl:arglist_info(u_scenario_desirability, f_u_scenario_desirability, [u_context, u_top_level_goal], arginfo{all:[u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_top_level_goal], opt:0, req:[u_context, u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_scenario_desirability,
			  wl:init_args(exact_only, f_u_scenario_desirability))).
*/
/*
*/
/*
 This is called upon top-level goal failure or success.
*/
/*
*/
/*
 Todo: don't store episode if derived from another episode by
*/
/*
 analogy without repairs.
*/
/*
*/
/*
 Todo: use situation assumptions as indices?
*/
/*
*/
/*
(defun episode-store-top-goal (top-level-goal context)
  (ndbg-roman *gate-dbg* rule "Store episode")
  (ndbg-roman *gate-dbg* rule " "(defun episode-store-top-goal (top-level-goal context)\n  (ndbg-roman *gate-dbg* rule \"Store episode\")\n  (ndbg-roman *gate-dbg* rule \" ~A in ~A\" top-level-goal context)\n  (ndbg-newline *gate-dbg* rule)\n  (no-gen\n  (let ((desirability (scenario-desirability context top-level-goal))\n        (ep nil)\n        (result-emot (goal->result-emotion top-level-goal *reality*)))\n       (if (ty$instance? (ob$get top-level-goal 'obj) 'skipindex)\n           (setq ep (episode-store1 (car (goal-subgoals top-level-goal context\n                                                       *me-belief-path*))\n                                   context desirability))\n           (setq ep (episode-store1 top-level-goal context desirability)))\n       (if ep\n           (progn\n            (if result-emot (epmem-store ep result-emot nil nil))\n            ; Todo: index under causing state (personal goal)\n            ;       or causing emotion (daydreaming goal).\n            ; In below, indices are needed neither for plan nor for\n            ; reminding. But wouldn't we like to make it require, say,\n            ; half the misc indices for a reminding?\n            (yloop (yfor index in (find-misc-indices context top-level-goal))\n                   (ydo (epmem-store ep index nil nil)))\n            ; Do housekeeping for a recent episode (but without the\n            ; reminding hoopla; also no serendipities since those\n            ; are run whenever a new rule is induced).\n            (epmem-reminding ep t t)\n            ep)\n           nil))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-store-top-goal',['top-level-goal',context],['ndbg-roman','*gate-dbg*',rule,'$STRING'("Store episode")],['ndbg-roman','*gate-dbg*',rule,'$STRING'(" ~A in ~A"),'top-level-goal',context],['ndbg-newline','*gate-dbg*',rule],['no-gen',[let,[[desirability,['scenario-desirability',context,'top-level-goal']],[ep,[]],['result-emot',['goal->result-emotion','top-level-goal','*reality*']]],[if,['ty$instance?',['ob$get','top-level-goal',[quote,obj]],[quote,skipindex]],[setq,ep,['episode-store1',[car,['goal-subgoals','top-level-goal',context,'*me-belief-path*']],context,desirability]],[setq,ep,['episode-store1','top-level-goal',context,desirability]]],[if,ep,[progn,[if,'result-emot',['epmem-store',ep,'result-emot',[],[]]],[yloop,[yfor,index,in,['find-misc-indices',context,'top-level-goal']],[ydo,['epmem-store',ep,index,[],[]]]],['epmem-reminding',ep,t,t],ep],[]]]]])
wl:lambda_def(defun, u_episode_store_top_goal, f_u_episode_store_top_goal, [u_top_level_goal, u_context], [[u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Store episode")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A in ~A"), u_top_level_goal, u_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_no_gen, [let, [[u_desirability, [u_scenario_desirability, u_context, u_top_level_goal]], [u_ep, []], [u_result_emot, [u_goal_c62_result_emotion, u_top_level_goal, u_xx_reality_xx]]], [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_top_level_goal, [quote, u_obj]], [quote, u_skipindex]], [setq, u_ep, [u_episode_store1, [car, [u_goal_subgoals, u_top_level_goal, u_context, u_xx_me_belief_path_xx]], u_context, u_desirability]], [setq, u_ep, [u_episode_store1, u_top_level_goal, u_context, u_desirability]]], [if, u_ep, [progn, [if, u_result_emot, [u_epmem_store, u_ep, u_result_emot, [], []]], [u_yloop, [u_yfor, index, u_in, [u_find_misc_indices, u_context, u_top_level_goal]], [u_ydo, [u_epmem_store, u_ep, index, [], []]]], [u_epmem_reminding, u_ep, t, t], u_ep], []]]]]).
wl:arglist_info(u_episode_store_top_goal, f_u_episode_store_top_goal, [u_top_level_goal, u_context], arginfo{all:[u_top_level_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_context], opt:0, req:[u_top_level_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_store_top_goal).

/*

### Compiled:  `U::EPISODE-STORE-TOP-GOAL` 
*/
f_u_episode_store_top_goal(Top_level_goal, Context, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_top_level_goal, Top_level_goal), bv(u_context, Context)|Env],
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       ['$ARRAY'([*], claz_base_character, "Store episode")],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*], claz_base_character, " ~A in ~A"),
			 u_top_level_goal,
			 u_context
		       ],
		       Ndbg_roman_Ret11),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_no_gen(
		   [ 
		     [ let,
		       
		       [ 
			 [ u_desirability,
			   
			   [ u_scenario_desirability,
			     u_context,
			     u_top_level_goal
			   ]
			 ],
			 [u_ep, []],
			 
			 [ u_result_emot,
			   
			   [ u_goal_c62_result_emotion,
			     u_top_level_goal,
			     u_xx_reality_xx
			   ]
			 ]
		       ],
		       
		       [ if,
			 
			 [ u_ty_c36_instance_c63,
			   [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
			   [quote, u_skipindex]
			 ],
			 
			 [ setq,
			   u_ep,
			   
			   [ u_episode_store1,
			     
			     [ car,
			       
			       [ u_goal_subgoals,
				 u_top_level_goal,
				 u_context,
				 u_xx_me_belief_path_xx
			       ]
			     ],
			     u_context,
			     u_desirability
			   ]
			 ],
			 
			 [ setq,
			   u_ep,
			   
			   [ u_episode_store1,
			     u_top_level_goal,
			     u_context,
			     u_desirability
			   ]
			 ]
		       ],
		       
		       [ if,
			 u_ep,
			 
			 [ progn,
			   
			   [ if,
			     u_result_emot,
			     [u_epmem_store, u_ep, u_result_emot, [], []]
			   ],
			   
			   [ u_yloop,
			     
			     [ u_yfor,
			       index,
			       u_in,
			       
			       [ u_find_misc_indices,
				 u_context,
				 u_top_level_goal
			       ]
			     ],
			     [u_ydo, [u_epmem_store, u_ep, index, [], []]]
			   ],
			   [u_epmem_reminding, u_ep, t, t],
			   u_ep
			 ],
			 []
		       ]
		     ]
		   ],
		   No_gen_Ret),
	No_gen_Ret=FnResult.
:- set_opv(f_u_episode_store_top_goal, classof, claz_function),
   set_opv(u_episode_store_top_goal, compile_as, kw_function),
   set_opv(u_episode_store_top_goal, function, f_u_episode_store_top_goal),
   DefunResult=u_episode_store_top_goal.
/*
:- side_effect(assert_lsp(u_episode_store_top_goal,
			  wl:lambda_def(defun, u_episode_store_top_goal, f_u_episode_store_top_goal, [u_top_level_goal, u_context], [[u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Store episode")], [u_ndbg_roman, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, " ~A in ~A"), u_top_level_goal, u_context], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_no_gen, [let, [[u_desirability, [u_scenario_desirability, u_context, u_top_level_goal]], [u_ep, []], [u_result_emot, [u_goal_c62_result_emotion, u_top_level_goal, u_xx_reality_xx]]], [if, [u_ty_c36_instance_c63, [u_ob_c36_get, u_top_level_goal, [quote, u_obj]], [quote, u_skipindex]], [setq, u_ep, [u_episode_store1, [car, [u_goal_subgoals, u_top_level_goal, u_context, u_xx_me_belief_path_xx]], u_context, u_desirability]], [setq, u_ep, [u_episode_store1, u_top_level_goal, u_context, u_desirability]]], [if, u_ep, [progn, [if, u_result_emot, [u_epmem_store, u_ep, u_result_emot, [], []]], [u_yloop, [u_yfor, index, u_in, [u_find_misc_indices, u_context, u_top_level_goal]], [u_ydo, [u_epmem_store, u_ep, index, [], []]]], [u_epmem_reminding, u_ep, t, t], u_ep], []]]]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_store_top_goal,
			  wl:arglist_info(u_episode_store_top_goal, f_u_episode_store_top_goal, [u_top_level_goal, u_context], arginfo{all:[u_top_level_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_top_level_goal, u_context], opt:0, req:[u_top_level_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_store_top_goal,
			  wl:init_args(exact_only, f_u_episode_store_top_goal))).
*/
/*
 Todo: index under causing state (personal goal)
*/
/*
       or causing emotion (daydreaming goal).
*/
/*
 In below, indices are needed neither for plan nor for
*/
/*
 reminding. But wouldn't we like to make it require, say,
*/
/*
 half the misc indices for a reminding?
*/
/*
 Do housekeeping for a recent episode (but without the
*/
/*
 reminding hoopla; also no serendipities since those
*/
/*
 are run whenever a new rule is induced).
*/
/*
(defun goal->result-emotion (goal context)
  (let ((result 
         (prune (ol-get goal *dependency-ob* 'forward context)
                (lambda (x) (ty$instance? x 'emotion)))))
       (if result
           (progn
            ; Checks to be removed once they seem to hold.
            (if (cdr result)
                (error "More than one result emotion for "(defun goal->result-emotion (goal context)\n  (let ((result \n         (prune (ol-get goal *dependency-ob* 'forward context)\n                (lambda (x) (ty$instance? x 'emotion)))))\n       (if result\n           (progn\n            ; Checks to be removed once they seem to hold.\n            (if (cdr result)\n                (error \"More than one result emotion for ~A?\" goal))\n;            (if (any? (lambda (x) (not (ty$instance? x 'emotion)))\n;                      result)\n;                (error \"Not all of ~A are emotions!!\" result))\n            (car result))\n           nil)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'goal->result-emotion',[goal,context],[let,[[result,[prune,['ol-get',goal,'*dependency-ob*',[quote,forward],context],[lambda,[x],['ty$instance?',x,[quote,emotion]]]]]],[if,result,[progn,[if,[cdr,result],[error,'$STRING'("More than one result emotion for ~A?"),goal]],[car,result]],[]]]])
wl:lambda_def(defun, u_goal_c62_result_emotion, f_u_goal_c62_result_emotion, [u_goal, u_context], [[let, [[u_result, [u_prune, [u_ol_get, u_goal, u_xx_dependency_ob_xx, [quote, u_forward], u_context], [lambda, [u_x], [u_ty_c36_instance_c63, u_x, [quote, u_emotion]]]]]], [if, u_result, [progn, [if, [cdr, u_result], [error, '$ARRAY'([*], claz_base_character, "More than one result emotion for ~A?"), u_goal]], [car, u_result]], []]]]).
wl:arglist_info(u_goal_c62_result_emotion, f_u_goal_c62_result_emotion, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_goal_c62_result_emotion).

/*

### Compiled:  `U::GOAL->RESULT-EMOTION` 
*/
f_u_goal_c62_result_emotion(Goal, Context, FnResult) :-
	nop(global_env(Env)),
	Env27=[bv(u_goal, Goal), bv(u_context, Context)|Env],
	get_var(Env27, u_context, Context_Get),
	get_var(Env27, u_goal, Goal_Get),
	get_var(Env27, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get(Goal_Get,
		   Xx_dependency_ob_xx_Get,
		   u_forward,
		   Context_Get,
		   Prune_Param),
	Lambda=closure([ClosureEnvironment|Env27], LResult, [u_x],  (get_var(ClosureEnvironment, u_x, X_Get), f_u_ty_c36_instance_c63(X_Get, u_emotion, LResult))),
	f_u_prune(Prune_Param, Lambda, Result_Init),
	LEnv=[bv(u_result, Result_Init)|Env27],
	get_var(LEnv, u_result, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_result, Result_Get20),
	    cl_cdr(Result_Get20, IFTEST18),
	    (   IFTEST18\==[]
	    ->  get_var(LEnv, u_goal, Goal_Get21),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "More than one result emotion for ~A?"),
			   Goal_Get21
			 ],
			 TrueResult),
		_274806074=TrueResult
	    ;   _274806074=[]
	    ),
	    get_var(LEnv, u_result, Result_Get23),
	    cl_car(Result_Get23, TrueResult24),
	    FnResult=TrueResult24
	;   FnResult=[]
	).
:- set_opv(f_u_goal_c62_result_emotion, classof, claz_function),
   set_opv(u_goal_c62_result_emotion, compile_as, kw_function),
   set_opv(u_goal_c62_result_emotion, function, f_u_goal_c62_result_emotion),
   DefunResult=u_goal_c62_result_emotion.
/*
:- side_effect(assert_lsp(u_goal_c62_result_emotion,
			  wl:lambda_def(defun, u_goal_c62_result_emotion, f_u_goal_c62_result_emotion, [u_goal, u_context], [[let, [[u_result, [u_prune, [u_ol_get, u_goal, u_xx_dependency_ob_xx, [quote, u_forward], u_context], [lambda, [u_x], [u_ty_c36_instance_c63, u_x, [quote, u_emotion]]]]]], [if, u_result, [progn, [if, [cdr, u_result], [error, '$ARRAY'([*], claz_base_character, "More than one result emotion for ~A?"), u_goal]], [car, u_result]], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_goal_c62_result_emotion,
			  wl:arglist_info(u_goal_c62_result_emotion, f_u_goal_c62_result_emotion, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_goal_c62_result_emotion,
			  wl:init_args(exact_only, f_u_goal_c62_result_emotion))).
*/
/*
 Checks to be removed once they seem to hold.
*/
/*
            (if (any? (lambda (x) (not (ty$instance? x 'emotion)))
*/
/*
                      result)
*/
/*
                (error "Not all of "                (error \"Not all of ~A are emotions!!\" result))".
*/
/*
(defun goal->motiv-emotion (goal context)
  (ob$get goal 'main-motiv))

; This is expensive and should only be called upon indexing of
; a final episode.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21682 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'goal->motiv-emotion',[goal,context],['ob$get',goal,[quote,'main-motiv']]])
wl:lambda_def(defun, u_goal_c62_motiv_emotion, f_u_goal_c62_motiv_emotion, [u_goal, u_context], [[u_ob_c36_get, u_goal, [quote, u_main_motiv]]]).
wl:arglist_info(u_goal_c62_motiv_emotion, f_u_goal_c62_motiv_emotion, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_goal_c62_motiv_emotion).

/*

### Compiled:  `U::GOAL->MOTIV-EMOTION` 
*/
f_u_goal_c62_motiv_emotion(Goal, Context, FnResult) :-
	nop(global_env(Env)),
	Env7=[bv(u_goal, Goal), bv(u_context, Context)|Env],
	get_var(Env7, u_goal, Goal_Get),
	f_u_ob_c36_get(Goal_Get, u_main_motiv, Main_motiv),
	Main_motiv=FnResult.
:- set_opv(f_u_goal_c62_motiv_emotion, classof, claz_function),
   set_opv(u_goal_c62_motiv_emotion, compile_as, kw_function),
   set_opv(u_goal_c62_motiv_emotion, function, f_u_goal_c62_motiv_emotion),
   DefunResult=u_goal_c62_motiv_emotion.
/*
:- side_effect(assert_lsp(u_goal_c62_motiv_emotion,
			  wl:lambda_def(defun, u_goal_c62_motiv_emotion, f_u_goal_c62_motiv_emotion, [u_goal, u_context], [[u_ob_c36_get, u_goal, [quote, u_main_motiv]]]))).
*/
/*
:- side_effect(assert_lsp(u_goal_c62_motiv_emotion,
			  wl:arglist_info(u_goal_c62_motiv_emotion, f_u_goal_c62_motiv_emotion, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_goal_c62_motiv_emotion,
			  wl:init_args(exact_only, f_u_goal_c62_motiv_emotion))).
*/
/*
 This is expensive and should only be called upon indexing of
*/
/*
 a final episode.
*/
/*
(defun find-misc-indices (context top-level-goal)
  (yloop
   (initial (result nil)
            (ancestors
             (cx$ancestors 
              (ob$get top-level-goal 'activation-context))))
   (yfor ob in (cx$get-all context))
   (ydo
    ; First condition checks that ob was asserted in activation context
    ; or later.
    (if (and (not (any? (lambda (x) (memq? x ancestors))
                        (ob$gets ob 'top-context)))
             (or (ty$instance? ob 'goal)
                 (ty$instance? ob 'state)))
    ; Todo: should not include other common indices.
        (setq result
             (prune (union result (objects-in ob))
                    (lambda (elem) (not (me? elem)))))))
   (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21836 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'find-misc-indices',[context,'top-level-goal'],[yloop,[initial,[result,[]],[ancestors,['cx$ancestors',['ob$get','top-level-goal',[quote,'activation-context']]]]],[yfor,ob,in,['cx$get-all',context]],[ydo,[if,[and,[not,['any?',[lambda,[x],['memq?',x,ancestors]],['ob$gets',ob,[quote,'top-context']]]],[or,['ty$instance?',ob,[quote,goal]],['ty$instance?',ob,[quote,state]]]],[setq,result,[prune,[union,result,['objects-in',ob]],[lambda,[elem],[not,['me?',elem]]]]]]],[yresult,result]]])
wl:lambda_def(defun, u_find_misc_indices, f_u_find_misc_indices, [u_context, u_top_level_goal], [[u_yloop, [u_initial, [u_result, []], [u_ancestors, [u_cx_c36_ancestors, [u_ob_c36_get, u_top_level_goal, [quote, u_activation_context]]]]], [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]], [u_ydo, [if, [and, [not, [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_ancestors]], [u_ob_c36_gets, u_ob, [quote, u_top_context]]]], [or, [u_ty_c36_instance_c63, u_ob, [quote, u_goal]], [u_ty_c36_instance_c63, u_ob, [quote, u_state]]]], [setq, u_result, [u_prune, [union, u_result, [u_objects_in, u_ob]], [lambda, [u_elem], [not, [u_me_c63, u_elem]]]]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_find_misc_indices, f_u_find_misc_indices, [u_context, u_top_level_goal], arginfo{all:[u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_top_level_goal], opt:0, req:[u_context, u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_find_misc_indices).

/*

### Compiled:  `U::FIND-MISC-INDICES` 
*/
f_u_find_misc_indices(Context, Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_context, Context), bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      
		      [ u_ancestors,
			
			[ u_cx_c36_ancestors,
			  
			  [ u_ob_c36_get,
			    u_top_level_goal,
			    [quote, u_activation_context]
			  ]
			]
		      ]
		    ],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ not,
			    
			    [ u_any_c63,
			      [lambda, [u_x], [u_memq_c63, u_x, u_ancestors]],
			      [u_ob_c36_gets, u_ob, [quote, u_top_context]]
			    ]
			  ],
			  
			  [ or,
			    [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			    [u_ty_c36_instance_c63, u_ob, [quote, u_state]]
			  ]
			],
			
			[ setq,
			  u_result,
			  
			  [ u_prune,
			    [union, u_result, [u_objects_in, u_ob]],
			    [lambda, [u_elem], [not, [u_me_c63, u_elem]]]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_find_misc_indices, classof, claz_function),
   set_opv(u_find_misc_indices, compile_as, kw_function),
   set_opv(u_find_misc_indices, function, f_u_find_misc_indices),
   DefunResult=u_find_misc_indices.
/*
:- side_effect(assert_lsp(u_find_misc_indices,
			  wl:lambda_def(defun, u_find_misc_indices, f_u_find_misc_indices, [u_context, u_top_level_goal], [[u_yloop, [u_initial, [u_result, []], [u_ancestors, [u_cx_c36_ancestors, [u_ob_c36_get, u_top_level_goal, [quote, u_activation_context]]]]], [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]], [u_ydo, [if, [and, [not, [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_ancestors]], [u_ob_c36_gets, u_ob, [quote, u_top_context]]]], [or, [u_ty_c36_instance_c63, u_ob, [quote, u_goal]], [u_ty_c36_instance_c63, u_ob, [quote, u_state]]]], [setq, u_result, [u_prune, [union, u_result, [u_objects_in, u_ob]], [lambda, [u_elem], [not, [u_me_c63, u_elem]]]]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_find_misc_indices,
			  wl:arglist_info(u_find_misc_indices, f_u_find_misc_indices, [u_context, u_top_level_goal], arginfo{all:[u_context, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context, u_top_level_goal], opt:0, req:[u_context, u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_find_misc_indices,
			  wl:init_args(exact_only, f_u_find_misc_indices))).
*/
/*
 First condition checks that ob was asserted in activation context
*/
/*
 or later.
*/
/*
 Todo: should not include other common indices.
*/
/*
(defun episode-store1 (goal context desirability)
  (ndbg-roman-nl *gate-dbg* ep-store "Store goal of episode "(defun episode-store1 (goal context desirability)\n  (ndbg-roman-nl *gate-dbg* ep-store \"Store goal of episode ~A, realism ~A\"\n                 goal (strength (ob$get goal 'obj)))\n  (let* ((rule nil)\n         (subgoals (goal-subgoals goal context *me-belief-path*))\n         (ep nil))\n        (if subgoals\n            (progn\n             (setq rule (goal-subgoals-rule goal context *me-belief-path*))\n             (yloop (yfor subgoal in subgoals)\n                    (ydo (episode-store1 subgoal context nil)))\n             (setq ep (make-and-store-episode rule goal context\n                                             (strength (ob$get goal 'obj))\n                                             desirability nil\n                                             (subgoals->eps subgoals)))))\n        ep))\n\n;\n; Similarity metric for episodic retrieval\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:22564 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episode-store1',[goal,context,desirability],['ndbg-roman-nl','*gate-dbg*','ep-store','$STRING'("Store goal of episode ~A, realism ~A"),goal,[strength,['ob$get',goal,[quote,obj]]]],['let*',[[rule,[]],[subgoals,['goal-subgoals',goal,context,'*me-belief-path*']],[ep,[]]],[if,subgoals,[progn,[setq,rule,['goal-subgoals-rule',goal,context,'*me-belief-path*']],[yloop,[yfor,subgoal,in,subgoals],[ydo,['episode-store1',subgoal,context,[]]]],[setq,ep,['make-and-store-episode',rule,goal,context,[strength,['ob$get',goal,[quote,obj]]],desirability,[],['subgoals->eps',subgoals]]]]],ep]])
wl:lambda_def(defun, u_episode_store1, f_u_episode_store1, [u_goal, u_context, u_desirability], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Store goal of episode ~A, realism ~A"), u_goal, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]], [let_xx, [[u_rule, []], [u_subgoals, [u_goal_subgoals, u_goal, u_context, u_xx_me_belief_path_xx]], [u_ep, []]], [if, u_subgoals, [progn, [setq, u_rule, [u_goal_subgoals_rule, u_goal, u_context, u_xx_me_belief_path_xx]], [u_yloop, [u_yfor, u_subgoal, u_in, u_subgoals], [u_ydo, [u_episode_store1, u_subgoal, u_context, []]]], [setq, u_ep, [u_make_and_store_episode, u_rule, u_goal, u_context, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]], u_desirability, [], [u_subgoals_c62_eps, u_subgoals]]]]], u_ep]]).
wl:arglist_info(u_episode_store1, f_u_episode_store1, [u_goal, u_context, u_desirability], arginfo{all:[u_goal, u_context, u_desirability], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_desirability], opt:0, req:[u_goal, u_context, u_desirability], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episode_store1).

/*

### Compiled:  `U::EPISODE-STORE1` 
*/
f_u_episode_store1(Goal, Context, Desirability, FnResult) :-
	nop(global_env(Env)),
	Env27=[bv(u_goal, Goal), bv(u_context, Context), bv(u_desirability, Desirability)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Store goal of episode ~A, realism ~A"),
			    u_goal,
			    [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]
			  ],
			  Roman_nl_Ret),
	get_var(Env27, u_context, Context_Get),
	get_var(Env27, u_goal, Goal_Get),
	get_var(Env27, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_goal_subgoals(Goal_Get,
			  Context_Get,
			  Xx_me_belief_path_xx_Get,
			  Subgoals_Init),
	LEnv=[bv(u_rule, []), bv(u_subgoals, Subgoals_Init), bv(u_ep, [])|Env27],
	get_var(LEnv, u_subgoals, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_context, Context_Get16),
	    get_var(LEnv, u_goal, Goal_Get15),
	    get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get17),
	    f_u_goal_subgoals_rule(Goal_Get15,
				   Context_Get16,
				   Xx_me_belief_path_xx_Get17,
				   Rule),
	    set_var(LEnv, u_rule, Rule),
	    f_u_yloop(
		      [ [u_yfor, u_subgoal, u_in, u_subgoals],
			[u_ydo, [u_episode_store1, u_subgoal, u_context, []]]
		      ],
		      Yloop_Ret),
	    get_var(LEnv, u_context, Context_Get20),
	    get_var(LEnv, u_goal, Goal_Get19),
	    get_var(LEnv, u_rule, Rule_Get),
	    f_u_strength([u_ob_c36_get, u_goal, [quote, u_obj]], Strength_Ret),
	    get_var(LEnv, u_desirability, Desirability_Get),
	    get_var(LEnv, u_subgoals, Subgoals_Get22),
	    f_u_subgoals_c62_eps(Subgoals_Get22, C62_eps_Ret),
	    f_u_make_and_store_episode(Rule_Get,
				       Goal_Get19,
				       Context_Get20,
				       Strength_Ret,
				       Desirability_Get,
				       [],
				       C62_eps_Ret,
				       TrueResult),
	    set_var(LEnv, u_ep, TrueResult),
	    _278492008=TrueResult
	;   _278492008=[]
	),
	get_var(LEnv, u_ep, Ep_Get),
	Ep_Get=FnResult.
:- set_opv(f_u_episode_store1, classof, claz_function),
   set_opv(u_episode_store1, compile_as, kw_function),
   set_opv(u_episode_store1, function, f_u_episode_store1),
   DefunResult=u_episode_store1.
/*
:- side_effect(assert_lsp(u_episode_store1,
			  wl:lambda_def(defun, u_episode_store1, f_u_episode_store1, [u_goal, u_context, u_desirability], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_ep_store, '$ARRAY'([*], claz_base_character, "Store goal of episode ~A, realism ~A"), u_goal, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]], [let_xx, [[u_rule, []], [u_subgoals, [u_goal_subgoals, u_goal, u_context, u_xx_me_belief_path_xx]], [u_ep, []]], [if, u_subgoals, [progn, [setq, u_rule, [u_goal_subgoals_rule, u_goal, u_context, u_xx_me_belief_path_xx]], [u_yloop, [u_yfor, u_subgoal, u_in, u_subgoals], [u_ydo, [u_episode_store1, u_subgoal, u_context, []]]], [setq, u_ep, [u_make_and_store_episode, u_rule, u_goal, u_context, [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]], u_desirability, [], [u_subgoals_c62_eps, u_subgoals]]]]], u_ep]]))).
*/
/*
:- side_effect(assert_lsp(u_episode_store1,
			  wl:arglist_info(u_episode_store1, f_u_episode_store1, [u_goal, u_context, u_desirability], arginfo{all:[u_goal, u_context, u_desirability], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, u_desirability], opt:0, req:[u_goal, u_context, u_desirability], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episode_store1,
			  wl:init_args(exact_only, f_u_episode_store1))).
*/
/*
*/
/*
 Similarity metric for episodic retrieval
*/
/*
*/
/*
(defun ob$similarity (ob1 ob2)
  (ndbg-roman-nl *gate-dbg* simil
   "Assess similarity between "(defun ob$similarity (ob1 ob2)\n  (ndbg-roman-nl *gate-dbg* simil\n   \"Assess similarity between ~A and ~A\" ob1 ob2)\n  (let ((result (ob$similarity1 ob1 ob2)))\n    (ndbg-roman-nl *gate-dbg* simil\n     \"Similarity between ~A and ~A = ~A\" ob1 ob2\n          result)\n    result))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23412 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$similarity',[ob1,ob2],['ndbg-roman-nl','*gate-dbg*',simil,'$STRING'("Assess similarity between ~A and ~A"),ob1,ob2],[let,[[result,['ob$similarity1',ob1,ob2]]],['ndbg-roman-nl','*gate-dbg*',simil,'$STRING'("Similarity between ~A and ~A = ~A"),ob1,ob2,result],result]])
wl:lambda_def(defun, u_ob_c36_similarity, f_u_ob_c36_similarity, [u_ob1, u_ob2], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_simil, '$ARRAY'([*], claz_base_character, "Assess similarity between ~A and ~A"), u_ob1, u_ob2], [let, [[u_result, [u_ob_c36_similarity1, u_ob1, u_ob2]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_simil, '$ARRAY'([*], claz_base_character, "Similarity between ~A and ~A = ~A"), u_ob1, u_ob2, u_result], u_result]]).
wl:arglist_info(u_ob_c36_similarity, f_u_ob_c36_similarity, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_similarity).

/*

### Compiled:  `U::OB$SIMILARITY` 
*/
f_u_ob_c36_similarity(Ob1, Ob2, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_ob1, Ob1), bv(u_ob2, Ob2)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_simil,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Assess similarity between ~A and ~A"),
			    u_ob1,
			    u_ob2
			  ],
			  Roman_nl_Ret),
	get_var(Env13, u_ob1, Ob1_Get),
	get_var(Env13, u_ob2, Ob2_Get),
	f_u_ob_c36_similarity1(Ob1_Get, Ob2_Get, Result_Init),
	LEnv=[bv(u_result, Result_Init)|Env13],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_simil,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Similarity between ~A and ~A = ~A"),
			    u_ob1,
			    u_ob2,
			    u_result
			  ],
			  Roman_nl_Ret17),
	get_var(LEnv, u_result, Result_Get),
	Result_Get=FnResult.
:- set_opv(f_u_ob_c36_similarity, classof, claz_function),
   set_opv(u_ob_c36_similarity, compile_as, kw_function),
   set_opv(u_ob_c36_similarity, function, f_u_ob_c36_similarity),
   DefunResult=u_ob_c36_similarity.
/*
:- side_effect(assert_lsp(u_ob_c36_similarity,
			  wl:lambda_def(defun, u_ob_c36_similarity, f_u_ob_c36_similarity, [u_ob1, u_ob2], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_simil, '$ARRAY'([*], claz_base_character, "Assess similarity between ~A and ~A"), u_ob1, u_ob2], [let, [[u_result, [u_ob_c36_similarity1, u_ob1, u_ob2]]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_simil, '$ARRAY'([*], claz_base_character, "Similarity between ~A and ~A = ~A"), u_ob1, u_ob2, u_result], u_result]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_similarity,
			  wl:arglist_info(u_ob_c36_similarity, f_u_ob_c36_similarity, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_similarity,
			  wl:init_args(exact_only, f_u_ob_c36_similarity))).
*/
/*
(defun distance->similarity (x)
  (fl- 1.0 (fl* .25 (fixnum->flonum x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23687 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'distance->similarity',[x],['fl-',1.0,['fl*',0.25,['fixnum->flonum',x]]]])
wl:lambda_def(defun, u_distance_c62_similarity, f_u_distance_c62_similarity, [u_x], [[u_flc45, 1.0, [u_fl_xx, 0.25, [u_fixnum_c62_flonum, u_x]]]]).
wl:arglist_info(u_distance_c62_similarity, f_u_distance_c62_similarity, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_distance_c62_similarity).

/*

### Compiled:  `U::DISTANCE->SIMILARITY` 
*/
f_u_distance_c62_similarity(X, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_x, X)|Env],
	f_u_flc45(1.0, [u_fl_xx, 0.25, [u_fixnum_c62_flonum, u_x]], Flc45_Ret),
	Flc45_Ret=FnResult.
:- set_opv(f_u_distance_c62_similarity, classof, claz_function),
   set_opv(u_distance_c62_similarity, compile_as, kw_function),
   set_opv(u_distance_c62_similarity, function, f_u_distance_c62_similarity),
   DefunResult=u_distance_c62_similarity.
/*
:- side_effect(assert_lsp(u_distance_c62_similarity,
			  wl:lambda_def(defun, u_distance_c62_similarity, f_u_distance_c62_similarity, [u_x], [[u_flc45, 1.0, [u_fl_xx, 0.25, [u_fixnum_c62_flonum, u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_distance_c62_similarity,
			  wl:arglist_info(u_distance_c62_similarity, f_u_distance_c62_similarity, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_distance_c62_similarity,
			  wl:init_args(exact_only, f_u_distance_c62_similarity))).
*/
/*
(defun ty-distance (type1 type2)
  (if (and (ty? type1) (ty? type2))
      (ty$distance type1 type2)
      0))

;
; Todo: We have to take 1/type-distance, no? What was original alg in notebook?
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23762 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ty-distance',[type1,type2],[if,[and,['ty?',type1],['ty?',type2]],['ty$distance',type1,type2],0]])
wl:lambda_def(defun, u_ty_distance, f_u_ty_distance, [u_type1, u_type2], [[if, [and, [u_ty_c63, u_type1], [u_ty_c63, u_type2]], [u_ty_c36_distance, u_type1, u_type2], 0]]).
wl:arglist_info(u_ty_distance, f_u_ty_distance, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_distance).

/*

### Compiled:  `U::TY-DISTANCE` 
*/
f_u_ty_distance(Type1, Type2, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_type1, Type1), bv(u_type2, Type2)|Env],
	f_u_ty_c63(u_type1, IFTEST6),
	(   IFTEST6\==[]
	->  f_u_ty_c63(u_type2, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env14, u_type1, Type1_Get),
	    get_var(Env14, u_type2, Type2_Get),
	    f_u_ty_c36_distance(Type1_Get, Type2_Get, TrueResult11),
	    FnResult=TrueResult11
	;   FnResult=0
	).
:- set_opv(f_u_ty_distance, classof, claz_function),
   set_opv(u_ty_distance, compile_as, kw_function),
   set_opv(u_ty_distance, function, f_u_ty_distance),
   DefunResult=u_ty_distance.
/*
:- side_effect(assert_lsp(u_ty_distance,
			  wl:lambda_def(defun, u_ty_distance, f_u_ty_distance, [u_type1, u_type2], [[if, [and, [u_ty_c63, u_type1], [u_ty_c63, u_type2]], [u_ty_c36_distance, u_type1, u_type2], 0]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_distance,
			  wl:arglist_info(u_ty_distance, f_u_ty_distance, [u_type1, u_type2], arginfo{all:[u_type1, u_type2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_type1, u_type2], opt:0, req:[u_type1, u_type2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_distance,
			  wl:init_args(exact_only, f_u_ty_distance))).
*/
/*
*/
/*
 Todo: We have to take 1/type-distance, no? What was original alg in notebook?
*/
/*
*/
/*
(defun ob$similarity1 (ob1 ob2)
  (cond
   ((eq? ob1 ob2) 1.0)
   ((or (pair? ob1) (symbol? ob1) (string? ob1)
        (pair? ob2) (symbol? ob2) (string? ob2)) 0.0) ; should do for now
   ((and (var? ob1) (var? ob2))
    (distance->similarity (ty-distance (variable-type ob1)
                                   (variable-type ob2))))
   ((var? ob1)
    (distance->similarity (ty-distance (variable-type ob1)
                                   (ob$ty ob2))))
   ((var? ob2)
    (distance->similarity (ty-distance (ob$ty ob1)
                                   (variable-type ob2))))
   ((and (ty$instance? ob1 'object)
         (ty$instance? ob2 'object))
    (distance->similarity (ty-distance (ob$ty ob1)
                                   (ob$ty ob2))))
   ((and (ob? ob1) (ob? ob2))
    (yloop (initial (val2 nil)
                   (temp nil)
                   (result (distance->similarity (ty-distance (ob$ty ob1)
                                                          (ob$ty ob2)))))
          (yuntil (= result *min-flonum*))
          ; Todo: Could keep track of used slots as in unify?
          (yfor sv in (ob$pairs ob1))
          ; below code really assumes no multiple slot values
          (ydo (if (and (neq? (slots-name sv) 'type)
                       (neq? (slots-name sv) 'strength)
                       (not (memq? (slots-name sv)
                                   *permanent-ignore-slots*)))
                  (progn
                   (setq val2 (ob$gets ob2 (slots-name sv)))
                   (if (null? val2)
                       (setq result *min-flonum*) ; non-homomorphic obs
                       (progn
                        (setq temp (apply 'max
                                         (map 'list (lambda (x) (ob$similarity1
                                                           (slots-value sv) x))
                                              val2)))
                        (if (not (= temp *min-flonum*))
                            (setq result (fl+ result (fl* 0.5 temp)))
                            (setq result *min-flonum*)))))))
           (yresult result)))
   (else (error "Bug: ob$similarity got unknown stuff: "(defun ob$similarity1 (ob1 ob2)\n  (cond\n   ((eq? ob1 ob2) 1.0)\n   ((or (pair? ob1) (symbol? ob1) (string? ob1)\n        (pair? ob2) (symbol? ob2) (string? ob2)) 0.0) ; should do for now\n   ((and (var? ob1) (var? ob2))\n    (distance->similarity (ty-distance (variable-type ob1)\n                                   (variable-type ob2))))\n   ((var? ob1)\n    (distance->similarity (ty-distance (variable-type ob1)\n                                   (ob$ty ob2))))\n   ((var? ob2)\n    (distance->similarity (ty-distance (ob$ty ob1)\n                                   (variable-type ob2))))\n   ((and (ty$instance? ob1 'object)\n         (ty$instance? ob2 'object))\n    (distance->similarity (ty-distance (ob$ty ob1)\n                                   (ob$ty ob2))))\n   ((and (ob? ob1) (ob? ob2))\n    (yloop (initial (val2 nil)\n                   (temp nil)\n                   (result (distance->similarity (ty-distance (ob$ty ob1)\n                                                          (ob$ty ob2)))))\n          (yuntil (= result *min-flonum*))\n          ; Todo: Could keep track of used slots as in unify?\n          (yfor sv in (ob$pairs ob1))\n          ; below code really assumes no multiple slot values\n          (ydo (if (and (neq? (slots-name sv) 'type)\n                       (neq? (slots-name sv) 'strength)\n                       (not (memq? (slots-name sv)\n                                   *permanent-ignore-slots*)))\n                  (progn\n                   (setq val2 (ob$gets ob2 (slots-name sv)))\n                   (if (null? val2)\n                       (setq result *min-flonum*) ; non-homomorphic obs\n                       (progn\n                        (setq temp (apply 'max\n                                         (map 'list (lambda (x) (ob$similarity1\n                                                           (slots-value sv) x))\n                                              val2)))\n                        (if (not (= temp *min-flonum*))\n                            (setq result (fl+ result (fl* 0.5 temp)))\n                            (setq result *min-flonum*)))))))\n           (yresult result)))\n   (else (error \"Bug: ob$similarity got unknown stuff: ~A ~A\" ob1 ob2))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$similarity1',[ob1,ob2],[cond,[['eq?',ob1,ob2],1.0],[[or,['pair?',ob1],['symbol?',ob1],['string?',ob1],['pair?',ob2],['symbol?',ob2],['string?',ob2]],0.0],[[and,['var?',ob1],['var?',ob2]],['distance->similarity',['ty-distance',['variable-type',ob1],['variable-type',ob2]]]],[['var?',ob1],['distance->similarity',['ty-distance',['variable-type',ob1],['ob$ty',ob2]]]],[['var?',ob2],['distance->similarity',['ty-distance',['ob$ty',ob1],['variable-type',ob2]]]],[[and,['ty$instance?',ob1,[quote,object]],['ty$instance?',ob2,[quote,object]]],['distance->similarity',['ty-distance',['ob$ty',ob1],['ob$ty',ob2]]]],[[and,['ob?',ob1],['ob?',ob2]],[yloop,[initial,[val2,[]],[temp,[]],[result,['distance->similarity',['ty-distance',['ob$ty',ob1],['ob$ty',ob2]]]]],[yuntil,[=,result,'*min-flonum*']],[yfor,sv,in,['ob$pairs',ob1]],[ydo,[if,[and,['neq?',['slots-name',sv],[quote,type]],['neq?',['slots-name',sv],[quote,strength]],[not,['memq?',['slots-name',sv],'*permanent-ignore-slots*']]],[progn,[setq,val2,['ob$gets',ob2,['slots-name',sv]]],[if,['null?',val2],[setq,result,'*min-flonum*'],[progn,[setq,temp,[apply,[quote,max],[map,[quote,list],[lambda,[x],['ob$similarity1',['slots-value',sv],x]],val2]]],[if,[not,[=,temp,'*min-flonum*']],[setq,result,['fl+',result,['fl*',0.5,temp]]],[setq,result,'*min-flonum*']]]]]]],[yresult,result]]],[else,[error,'$STRING'("Bug: ob$similarity got unknown stuff: ~A ~A"),ob1,ob2]]]])
wl:lambda_def(defun, u_ob_c36_similarity1, f_u_ob_c36_similarity1, [u_ob1, u_ob2], [[cond, [[u_eq_c63, u_ob1, u_ob2], 1.0], [[or, [u_pair_c63, u_ob1], [u_symbol_c63, u_ob1], [u_string_c63, u_ob1], [u_pair_c63, u_ob2], [u_symbol_c63, u_ob2], [u_string_c63, u_ob2]], 0.0], [[and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]], [u_distance_c62_similarity, [u_ty_distance, [u_variable_type, u_ob1], [u_variable_type, u_ob2]]]], [[u_var_c63, u_ob1], [u_distance_c62_similarity, [u_ty_distance, [u_variable_type, u_ob1], [u_ob_c36_ty, u_ob2]]]], [[u_var_c63, u_ob2], [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_variable_type, u_ob2]]]], [[and, [u_ty_c36_instance_c63, u_ob1, [quote, u_object]], [u_ty_c36_instance_c63, u_ob2, [quote, u_object]]], [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_ob_c36_ty, u_ob2]]]], [[and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]], [u_yloop, [u_initial, [u_val2, []], [u_temp, []], [u_result, [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_ob_c36_ty, u_ob2]]]]], [u_yuntil, [=, u_result, u_xx_min_flonum_xx]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob1]], [u_ydo, [if, [and, [u_neq_c63, [u_slots_name, u_sv], [quote, type]], [u_neq_c63, [u_slots_name, u_sv], [quote, u_strength]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_val2, [u_ob_c36_gets, u_ob2, [u_slots_name, u_sv]]], [if, [u_null_c63, u_val2], [setq, u_result, u_xx_min_flonum_xx], [progn, [setq, u_temp, [apply, [quote, max], [map, [quote, list], [lambda, [u_x], [u_ob_c36_similarity1, [u_slots_value, u_sv], u_x]], u_val2]]], [if, [not, [=, u_temp, u_xx_min_flonum_xx]], [setq, u_result, [u_fl_c43, u_result, [u_fl_xx, 0.5, u_temp]]], [setq, u_result, u_xx_min_flonum_xx]]]]]]], [u_yresult, u_result]]], [u_else, [error, '$ARRAY'([*], claz_base_character, "Bug: ob$similarity got unknown stuff: ~A ~A"), u_ob1, u_ob2]]]]).
wl:arglist_info(u_ob_c36_similarity1, f_u_ob_c36_similarity1, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_similarity1).

/*

### Compiled:  `U::OB$SIMILARITY1` 
*/
f_u_ob_c36_similarity1(Ob1, Ob2, ElseResult42) :-
	nop(global_env(Env)),
	Env55=[bv(u_ob1, Ob1), bv(u_ob2, Ob2)|Env],
	f_u_eq_c63(u_ob1, u_ob2, IFTEST),
	(   IFTEST\==[]
	->  ElseResult42=1.0
	;   (   f_u_pair_c63(u_ob1, FORM1_Res12),
		FORM1_Res12\==[],
		IFTEST6=FORM1_Res12
	    ->  true
	    ;   f_u_symbol_c63(u_ob1, FORM1_Res11),
		FORM1_Res11\==[],
		IFTEST6=FORM1_Res11
	    ->  true
	    ;   f_u_string_c63(u_ob1, FORM1_Res10),
		FORM1_Res10\==[],
		IFTEST6=FORM1_Res10
	    ->  true
	    ;   f_u_pair_c63(u_ob2, FORM1_Res9),
		FORM1_Res9\==[],
		IFTEST6=FORM1_Res9
	    ->  true
	    ;   f_u_symbol_c63(u_ob2, FORM1_Res),
		FORM1_Res\==[],
		IFTEST6=FORM1_Res
	    ->  true
	    ;   f_u_string_c63(u_ob2, String_c63_Ret),
		IFTEST6=String_c63_Ret
	    ),
	    (   IFTEST6\==[]
	    ->  ElseResult42=0.0
	    ;   f_u_var_c63(u_ob1, IFTEST15),
		(   IFTEST15\==[]
		->  f_u_var_c63(u_ob2, TrueResult),
		    IFTEST13=TrueResult
		;   IFTEST13=[]
		),
		(   IFTEST13\==[]
		->  f_u_variable_type(u_ob1, Ty_distance_Param),
		    f_u_variable_type(u_ob2, Variable_type_Ret),
		    f_u_ty_distance(Ty_distance_Param,
				    Variable_type_Ret,
				    C62_similarity_Param),
		    f_u_distance_c62_similarity(C62_similarity_Param,
						TrueResult49),
		    ElseResult42=TrueResult49
		;   f_u_var_c63(u_ob1, IFTEST18),
		    (   IFTEST18\==[]
		    ->  f_u_variable_type(u_ob1, Ty_distance_Param60),
			f_u_ob_c36_ty(u_ob2, C36_ty_Ret),
			f_u_ty_distance(Ty_distance_Param60,
					C36_ty_Ret,
					C62_similarity_Param61),
			f_u_distance_c62_similarity(C62_similarity_Param61,
						    TrueResult47),
			ElseResult42=TrueResult47
		    ;   f_u_var_c63(u_ob2, IFTEST20),
			(   IFTEST20\==[]
			->  f_u_ob_c36_ty(u_ob1, Ty_distance_Param62),
			    f_u_variable_type(u_ob2, Variable_type_Ret69),
			    f_u_ty_distance(Ty_distance_Param62,
					    Variable_type_Ret69,
					    C62_similarity_Param63),
			    f_u_distance_c62_similarity(C62_similarity_Param63,
							TrueResult45),
			    ElseResult42=TrueResult45
			;   get_var(Env55, u_ob1, Ob1_Get),
			    f_u_ty_c36_instance_c63(Ob1_Get, u_object, IFTEST24),
			    (   IFTEST24\==[]
			    ->  get_var(Env55, u_ob2, Ob2_Get),
				f_u_ty_c36_instance_c63(Ob2_Get,
							u_object,
							TrueResult28),
				IFTEST22=TrueResult28
			    ;   IFTEST22=[]
			    ),
			    (   IFTEST22\==[]
			    ->  f_u_ob_c36_ty(u_ob1, Ty_distance_Param64),
				f_u_ob_c36_ty(u_ob2, C36_ty_Ret70),
				f_u_ty_distance(Ty_distance_Param64,
						C36_ty_Ret70,
						C62_similarity_Param65),
				f_u_distance_c62_similarity(C62_similarity_Param65,
							    TrueResult43),
				ElseResult42=TrueResult43
			    ;   f_u_ob_c63(u_ob1, IFTEST31),
				(   IFTEST31\==[]
				->  f_u_ob_c63(u_ob2, TrueResult33),
				    IFTEST29=TrueResult33
				;   IFTEST29=[]
				),
				(   IFTEST29\==[]
				->  f_u_yloop(
					      [ 
						[ u_initial,
						  [u_val2, []],
						  [u_temp, []],
						  
						  [ u_result,
						    
						    [ u_distance_c62_similarity,
						      
						      [ u_ty_distance,
							[u_ob_c36_ty, u_ob1],
							[u_ob_c36_ty, u_ob2]
						      ]
						    ]
						  ]
						],
						
						[ u_yuntil,
						  
						  [ (=),
						    u_result,
						    u_xx_min_flonum_xx
						  ]
						],
						
						[ u_yfor,
						  u_sv,
						  u_in,
						  [u_ob_c36_pairs, u_ob1]
						],
						
						[ u_ydo,
						  
						  [ if,
						    
						    [ and,
						      
						      [ u_neq_c63,
							[u_slots_name, u_sv],
							[quote, type]
						      ],
						      
						      [ u_neq_c63,
							[u_slots_name, u_sv],
							[quote, u_strength]
						      ],
						      
						      [ not,
							
							[ u_memq_c63,
							  [u_slots_name, u_sv],
							  u_xx_permanent_ignore_slots_xx
							]
						      ]
						    ],
						    
						    [ progn,
						      
						      [ setq,
							u_val2,
							
							[ u_ob_c36_gets,
							  u_ob2,
							  [u_slots_name, u_sv]
							]
						      ],
						      
						      [ if,
							[u_null_c63, u_val2],
							
							[ setq,
							  u_result,
							  u_xx_min_flonum_xx
							],
							
							[ progn,
							  
							  [ setq,
							    u_temp,
							    
							    [ apply,
							      [quote, max],
							      
							      [ map,
								[quote, list],
								
								[ lambda,
								  [u_x],
								  
								  [ u_ob_c36_similarity1,
								    
								    [ u_slots_value,
								      u_sv
								    ],
								    u_x
								  ]
								],
								u_val2
							      ]
							    ]
							  ],
							  
							  [ if,
							    
							    [ not,
							      
							      [ (=),
								u_temp,
								u_xx_min_flonum_xx
							      ]
							    ],
							    
							    [ setq,
							      u_result,
							      
							      [ u_fl_c43,
								u_result,
								
								[ u_fl_xx,
								  0.5,
								  u_temp
								]
							      ]
							    ],
							    
							    [ setq,
							      u_result,
							      u_xx_min_flonum_xx
							    ]
							  ]
							]
						      ]
						    ]
						  ]
						],
						[u_yresult, u_result]
					      ],
					      TrueResult41),
				    ElseResult42=TrueResult41
				;   get_var(Env55, u_else, IFTEST34),
				    (   IFTEST34\==[]
				    ->  get_var(Env55, u_ob1, Ob1_Get37),
					get_var(Env55, u_ob2, Ob2_Get38),
					cl_error(
						 [ '$ARRAY'([*],
							    claz_base_character,
							    "Bug: ob$similarity got unknown stuff: ~A ~A"),
						   Ob1_Get37,
						   Ob2_Get38
						 ],
						 TrueResult39),
					ElseResult42=TrueResult39
				    ;   ElseResult42=[]
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_similarity1, classof, claz_function),
   set_opv(u_ob_c36_similarity1, compile_as, kw_function),
   set_opv(u_ob_c36_similarity1, function, f_u_ob_c36_similarity1),
   DefunResult=u_ob_c36_similarity1.
/*
:- side_effect(assert_lsp(u_ob_c36_similarity1,
			  wl:lambda_def(defun, u_ob_c36_similarity1, f_u_ob_c36_similarity1, [u_ob1, u_ob2], [[cond, [[u_eq_c63, u_ob1, u_ob2], 1.0], [[or, [u_pair_c63, u_ob1], [u_symbol_c63, u_ob1], [u_string_c63, u_ob1], [u_pair_c63, u_ob2], [u_symbol_c63, u_ob2], [u_string_c63, u_ob2]], 0.0], [[and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]], [u_distance_c62_similarity, [u_ty_distance, [u_variable_type, u_ob1], [u_variable_type, u_ob2]]]], [[u_var_c63, u_ob1], [u_distance_c62_similarity, [u_ty_distance, [u_variable_type, u_ob1], [u_ob_c36_ty, u_ob2]]]], [[u_var_c63, u_ob2], [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_variable_type, u_ob2]]]], [[and, [u_ty_c36_instance_c63, u_ob1, [quote, u_object]], [u_ty_c36_instance_c63, u_ob2, [quote, u_object]]], [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_ob_c36_ty, u_ob2]]]], [[and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]], [u_yloop, [u_initial, [u_val2, []], [u_temp, []], [u_result, [u_distance_c62_similarity, [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_ob_c36_ty, u_ob2]]]]], [u_yuntil, [=, u_result, u_xx_min_flonum_xx]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob1]], [u_ydo, [if, [and, [u_neq_c63, [u_slots_name, u_sv], [quote, type]], [u_neq_c63, [u_slots_name, u_sv], [quote, u_strength]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_val2, [u_ob_c36_gets, u_ob2, [u_slots_name, u_sv]]], [if, [u_null_c63, u_val2], [setq, u_result, u_xx_min_flonum_xx], [progn, [setq, u_temp, [apply, [quote, max], [map, [quote, list], [lambda, [u_x], [u_ob_c36_similarity1, [u_slots_value, u_sv], u_x]], u_val2]]], [if, [not, [=, u_temp, u_xx_min_flonum_xx]], [setq, u_result, [u_fl_c43, u_result, [u_fl_xx, 0.5, u_temp]]], [setq, u_result, u_xx_min_flonum_xx]]]]]]], [u_yresult, u_result]]], [u_else, [error, '$ARRAY'([*], claz_base_character, "Bug: ob$similarity got unknown stuff: ~A ~A"), u_ob1, u_ob2]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_similarity1,
			  wl:arglist_info(u_ob_c36_similarity1, f_u_ob_c36_similarity1, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_similarity1,
			  wl:init_args(exact_only, f_u_ob_c36_similarity1))).
*/
/*
 should do for now
*/
/*
 Todo: Could keep track of used slots as in unify?
*/
/*
 below code really assumes no multiple slot values
*/
/*
 non-homomorphic obs
*/
/*
(setq *min-flonum* -10000.0) ; T has such a constant?

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:26158 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*min-flonum*',-10000.0])
:- set_var(AEnv, setq, u_xx_min_flonum_xx, -10000.0).
/*
 T has such a constant?
*/
/*
(defun try-analogical-plan (goal goal-obj context analogical-episode
                                           belief-path top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule-long "Try analogical plan for "(defun try-analogical-plan (goal goal-obj context analogical-episode\n                                           belief-path top-level-goal)\n  (ndbg-roman-nl *gate-dbg* rule-long \"Try analogical plan for ~A in ~A ep ~A\"\n        goal context analogical-episode)\n  (yloop\n   (initial (sprouted-contexts nil))\n   (yfor bd in (rule-applications goal-obj context\n                                 (ob$get analogical-episode 'rule)\n                                 belief-path nil))\n   (ydo (setq sprouted-contexts\n            (append! (run-analogical-plan goal goal-obj context bd\n                                          (ob$get analogical-episode 'goal)\n                                          (ob$get analogical-episode 'context)\n                                          (ob$get analogical-episode 'rule)\n                                          1.0 belief-path top-level-goal\n                                          analogical-episode nil)\n                     sprouted-contexts)))\n   (yresult sprouted-contexts)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:26213 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'try-analogical-plan',[goal,'goal-obj',context,'analogical-episode','belief-path','top-level-goal'],['ndbg-roman-nl','*gate-dbg*','rule-long','$STRING'("Try analogical plan for ~A in ~A ep ~A"),goal,context,'analogical-episode'],[yloop,[initial,['sprouted-contexts',[]]],[yfor,bd,in,['rule-applications','goal-obj',context,['ob$get','analogical-episode',[quote,rule]],'belief-path',[]]],[ydo,[setq,'sprouted-contexts',['append!',['run-analogical-plan',goal,'goal-obj',context,bd,['ob$get','analogical-episode',[quote,goal]],['ob$get','analogical-episode',[quote,context]],['ob$get','analogical-episode',[quote,rule]],1.0,'belief-path','top-level-goal','analogical-episode',[]],'sprouted-contexts']]],[yresult,'sprouted-contexts']]])
wl:lambda_def(defun, u_try_analogical_plan, f_u_try_analogical_plan, [u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Try analogical plan for ~A in ~A ep ~A"), u_goal, u_context, u_analogical_episode], [u_yloop, [u_initial, [u_sprouted_contexts, []]], [u_yfor, u_bd, u_in, [u_rule_applications, u_goal_obj, u_context, [u_ob_c36_get, u_analogical_episode, [quote, u_rule]], u_belief_path, []]], [u_ydo, [setq, u_sprouted_contexts, [u_append_c33, [u_run_analogical_plan, u_goal, u_goal_obj, u_context, u_bd, [u_ob_c36_get, u_analogical_episode, [quote, u_goal]], [u_ob_c36_get, u_analogical_episode, [quote, u_context]], [u_ob_c36_get, u_analogical_episode, [quote, u_rule]], 1.0, u_belief_path, u_top_level_goal, u_analogical_episode, []], u_sprouted_contexts]]], [u_yresult, u_sprouted_contexts]]]).
wl:arglist_info(u_try_analogical_plan, f_u_try_analogical_plan, [u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], arginfo{all:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], opt:0, req:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_try_analogical_plan).

/*

### Compiled:  `U::TRY-ANALOGICAL-PLAN` 
*/
f_u_try_analogical_plan(Goal, Goal_obj, Context, Analogical_episode, Belief_path, Top_level_goal, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_goal, Goal), bv(u_goal_obj, Goal_obj), bv(u_context, Context), bv(u_analogical_episode, Analogical_episode), bv(u_belief_path, Belief_path), bv(u_top_level_goal, Top_level_goal)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Try analogical plan for ~A in ~A ep ~A"),
			    u_goal,
			    u_context,
			    u_analogical_episode
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_sprouted_contexts, []]],
		    
		    [ u_yfor,
		      u_bd,
		      u_in,
		      
		      [ u_rule_applications,
			u_goal_obj,
			u_context,
			[u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
			u_belief_path,
			[]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_sprouted_contexts,
			
			[ u_append_c33,
			  
			  [ u_run_analogical_plan,
			    u_goal,
			    u_goal_obj,
			    u_context,
			    u_bd,
			    [u_ob_c36_get, u_analogical_episode, [quote, u_goal]],
			    
			    [ u_ob_c36_get,
			      u_analogical_episode,
			      [quote, u_context]
			    ],
			    [u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
			    1.0,
			    u_belief_path,
			    u_top_level_goal,
			    u_analogical_episode,
			    []
			  ],
			  u_sprouted_contexts
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_try_analogical_plan, classof, claz_function),
   set_opv(u_try_analogical_plan, compile_as, kw_function),
   set_opv(u_try_analogical_plan, function, f_u_try_analogical_plan),
   DefunResult=u_try_analogical_plan.
/*
:- side_effect(assert_lsp(u_try_analogical_plan,
			  wl:lambda_def(defun, u_try_analogical_plan, f_u_try_analogical_plan, [u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_long, '$ARRAY'([*], claz_base_character, "Try analogical plan for ~A in ~A ep ~A"), u_goal, u_context, u_analogical_episode], [u_yloop, [u_initial, [u_sprouted_contexts, []]], [u_yfor, u_bd, u_in, [u_rule_applications, u_goal_obj, u_context, [u_ob_c36_get, u_analogical_episode, [quote, u_rule]], u_belief_path, []]], [u_ydo, [setq, u_sprouted_contexts, [u_append_c33, [u_run_analogical_plan, u_goal, u_goal_obj, u_context, u_bd, [u_ob_c36_get, u_analogical_episode, [quote, u_goal]], [u_ob_c36_get, u_analogical_episode, [quote, u_context]], [u_ob_c36_get, u_analogical_episode, [quote, u_rule]], 1.0, u_belief_path, u_top_level_goal, u_analogical_episode, []], u_sprouted_contexts]]], [u_yresult, u_sprouted_contexts]]]))).
*/
/*
:- side_effect(assert_lsp(u_try_analogical_plan,
			  wl:arglist_info(u_try_analogical_plan, f_u_try_analogical_plan, [u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], arginfo{all:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], opt:0, req:[u_goal, u_goal_obj, u_context, u_analogical_episode, u_belief_path, u_top_level_goal], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_try_analogical_plan,
			  wl:init_args(exact_only, f_u_try_analogical_plan))).
*/
/*
(setq *relaxed-analogy-realism* 0.5)

; new vsn -- but doesn't do verification; that is left to
; try-analogical-plan since when analogical plan is first invoked,
; it is automatically verified.
;
; Todo: Need to deal with initial slot in here? (Note, however, possible
; conflictions with mutation4, etc.)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*relaxed-analogy-realism*',0.5])
:- set_var(AEnv, setq, u_xx_relaxed_analogy_realism_xx, 0.5).
/*
 new vsn -- but doesn't do verification; that is left to
*/
/*
 try-analogical-plan since when analogical plan is first invoked,
*/
/*
 it is automatically verified.
*/
/*
*/
/*
 Todo: Need to deal with initial slot in here? (Note, however, possible
*/
/*
 conflictions with mutation4, etc.)
*/
/*
(defun run-analogical-plan (target-goal target-goal-obj target-context
                             target-bd source-goal source-context rule
                             ordering belief-path top-level-goal
                             analogical-episode reminding?)
  (ndbg-roman-nl *gate-dbg* rule "Run analogical plan for "(defun run-analogical-plan (target-goal target-goal-obj target-context\n                             target-bd source-goal source-context rule\n                             ordering belief-path top-level-goal\n                             analogical-episode reminding?)\n  (ndbg-roman-nl *gate-dbg* rule \"Run analogical plan for ~A in ~A\" target-goal\n        target-context)\n  (let* ((source-subgoals (goal-subgoals source-goal source-context\n                                         *me-belief-path*))\n         (source-goal-obj (ob$get source-goal 'obj))\n         (r-subgoal-objs (rule-subgoal-objs rule))\n         (bd nil)\n         (sprouted-context nil)\n         (seq? (goal-subgoals-seq? source-goal source-context)))\n    (cond\n     ((null? source-subgoals)\n      (error \"I thought bottoming out was detected in activate-subgoal\")\n      (ndbg-roman-nl *gate-dbg* rule \"Analogical plan for ~A in ~A bottoms out\"\n            target-goal target-context)\n      nil)\n     (else\n      (setq sprouted-context (cx$sprout target-context))\n      (delay-dbgs sprouted-context\n       (set-ordering sprouted-context ordering)\n       (setq bd (episodic-unify (ob$get rule 'goal) r-subgoal-objs\n                               source-goal-obj source-subgoals target-goal\n                               sprouted-context target-bd belief-path t\n                               top-level-goal ; was target context\n                               analogical-episode))\n       (if (ob? (car bd))\n           (progn\n            (ndbg-roman-nl *gate-dbg* rule-xtra\n                           \"Resetting target goal from ~A to ~A\"\n             target-goal (car bd))\n            (setq target-goal (car bd))))\n       (if reminding?\n           (progn\n            (epmem-reminding analogical-episode nil nil)\n            (ndbg-roman-nl *gate-dbg* rule \"Apply episode ~A\"\n                           (ob->string analogical-episode))\n            (rule-fire-msg rule \"analogical plan\" target-context bd\n                           sprouted-context target-goal))\n           (progn\n            (ndbg-roman-nl *gate-dbg* rule \"Apply suggested episode ~A\"\n                           (ob->string analogical-episode))\n            (rule-fire-msg rule \"analogical plan\" target-context\n                           bd sprouted-context target-goal)\n            (ndbg-newline *gate-dbg* rule)))\n       (instan-and-activate-subgoals target-goal r-subgoal-objs bd rule\n                                     sprouted-context seq? source-subgoals\n                                     nil top-level-goal belief-path))\n      (list sprouted-context)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27540 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'run-analogical-plan',['target-goal','target-goal-obj','target-context','target-bd','source-goal','source-context',rule,ordering,'belief-path','top-level-goal','analogical-episode','reminding?'],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Run analogical plan for ~A in ~A"),'target-goal','target-context'],['let*',[['source-subgoals',['goal-subgoals','source-goal','source-context','*me-belief-path*']],['source-goal-obj',['ob$get','source-goal',[quote,obj]]],['r-subgoal-objs',['rule-subgoal-objs',rule]],[bd,[]],['sprouted-context',[]],['seq?',['goal-subgoals-seq?','source-goal','source-context']]],[cond,[['null?','source-subgoals'],[error,'$STRING'("I thought bottoming out was detected in activate-subgoal")],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Analogical plan for ~A in ~A bottoms out"),'target-goal','target-context'],[]],[else,[setq,'sprouted-context',['cx$sprout','target-context']],['delay-dbgs','sprouted-context',['set-ordering','sprouted-context',ordering],[setq,bd,['episodic-unify',['ob$get',rule,[quote,goal]],'r-subgoal-objs','source-goal-obj','source-subgoals','target-goal','sprouted-context','target-bd','belief-path',t,'top-level-goal','analogical-episode']],[if,['ob?',[car,bd]],[progn,['ndbg-roman-nl','*gate-dbg*','rule-xtra','$STRING'("Resetting target goal from ~A to ~A"),'target-goal',[car,bd]],[setq,'target-goal',[car,bd]]]],[if,'reminding?',[progn,['epmem-reminding','analogical-episode',[],[]],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Apply episode ~A"),['ob->string','analogical-episode']],['rule-fire-msg',rule,'$STRING'("analogical plan"),'target-context',bd,'sprouted-context','target-goal']],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Apply suggested episode ~A"),['ob->string','analogical-episode']],['rule-fire-msg',rule,'$STRING'("analogical plan"),'target-context',bd,'sprouted-context','target-goal'],['ndbg-newline','*gate-dbg*',rule]]],['instan-and-activate-subgoals','target-goal','r-subgoal-objs',bd,rule,'sprouted-context','seq?','source-subgoals',[],'top-level-goal','belief-path']],[list,'sprouted-context']]]]])
wl:lambda_def(defun, u_run_analogical_plan, f_u_run_analogical_plan, [u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Run analogical plan for ~A in ~A"), u_target_goal, u_target_context], [let_xx, [[u_source_subgoals, [u_goal_subgoals, u_source_goal, u_source_context, u_xx_me_belief_path_xx]], [u_source_goal_obj, [u_ob_c36_get, u_source_goal, [quote, u_obj]]], [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]], [u_bd, []], [u_sprouted_context, []], [u_seq_c63, [u_goal_subgoals_seq_c63, u_source_goal, u_source_context]]], [cond, [[u_null_c63, u_source_subgoals], [error, '$ARRAY'([*], claz_base_character, "I thought bottoming out was detected in activate-subgoal")], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Analogical plan for ~A in ~A bottoms out"), u_target_goal, u_target_context], []], [u_else, [setq, u_sprouted_context, [u_cx_c36_sprout, u_target_context]], [u_delay_dbgs, u_sprouted_context, [u_set_ordering, u_sprouted_context, u_ordering], [setq, u_bd, [u_episodic_unify, [u_ob_c36_get, u_rule, [quote, u_goal]], u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_sprouted_context, u_target_bd, u_belief_path, t, u_top_level_goal, u_analogical_episode]], [if, [u_ob_c63, [car, u_bd]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Resetting target goal from ~A to ~A"), u_target_goal, [car, u_bd]], [setq, u_target_goal, [car, u_bd]]]], [if, u_reminding_c63, [progn, [u_epmem_reminding, u_analogical_episode, [], []], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Apply episode ~A"), [u_ob_c62_string, u_analogical_episode]], [u_rule_fire_msg, u_rule, '$ARRAY'([*], claz_base_character, "analogical plan"), u_target_context, u_bd, u_sprouted_context, u_target_goal]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Apply suggested episode ~A"), [u_ob_c62_string, u_analogical_episode]], [u_rule_fire_msg, u_rule, '$ARRAY'([*], claz_base_character, "analogical plan"), u_target_context, u_bd, u_sprouted_context, u_target_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]]], [u_instan_and_activate_subgoals, u_target_goal, u_r_subgoal_objs, u_bd, u_rule, u_sprouted_context, u_seq_c63, u_source_subgoals, [], u_top_level_goal, u_belief_path]], [list, u_sprouted_context]]]]]).
wl:arglist_info(u_run_analogical_plan, f_u_run_analogical_plan, [u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], arginfo{all:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], opt:0, req:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_run_analogical_plan).

/*

### Compiled:  `U::RUN-ANALOGICAL-PLAN` 
*/
f_u_run_analogical_plan(Target_goal, Target_goal_obj, Target_context, Target_bd, Source_goal, Source_context, Rule, Ordering, Belief_path, Top_level_goal, Analogical_episode, Reminding_c63, ElseResult28) :-
	nop(global_env(Env)),
	Env31=[bv(u_target_goal, Target_goal), bv(u_target_goal_obj, Target_goal_obj), bv(u_target_context, Target_context), bv(u_target_bd, Target_bd), bv(u_source_goal, Source_goal), bv(u_source_context, Source_context), bv(u_rule, Rule), bv(u_ordering, Ordering), bv(u_belief_path, Belief_path), bv(u_top_level_goal, Top_level_goal), bv(u_analogical_episode, Analogical_episode), bv(u_reminding_c63, Reminding_c63)|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Run analogical plan for ~A in ~A"),
			    u_target_goal,
			    u_target_context
			  ],
			  Roman_nl_Ret),
	get_var(Env31, u_source_context, Source_context_Get),
	get_var(Env31, u_source_goal, Source_goal_Get),
	get_var(Env31, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_goal_subgoals(Source_goal_Get,
			  Source_context_Get,
			  Xx_me_belief_path_xx_Get,
			  Source_subgoals_Init),
	get_var(Env31, u_source_goal, Source_goal_Get10),
	f_u_ob_c36_get(Source_goal_Get10, u_obj, Source_goal_obj_Init),
	get_var(Env31, u_rule, Rule_Get),
	f_u_rule_subgoal_objs(Rule_Get, R_subgoal_objs_Init),
	get_var(Env31, u_source_context, Source_context_Get13),
	get_var(Env31, u_source_goal, Source_goal_Get12),
	f_u_goal_subgoals_seq_c63(Source_goal_Get12,
				  Source_context_Get13,
				  Seq_c63_Init),
	LEnv=[bv(u_source_subgoals, Source_subgoals_Init), bv(u_source_goal_obj, Source_goal_obj_Init), bv(u_r_subgoal_objs, R_subgoal_objs_Init), bv(u_bd, []), bv(u_sprouted_context, []), bv(u_seq_c63, Seq_c63_Init)|Env31],
	f_u_null_c63(u_source_subgoals, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"I thought bottoming out was detected in activate-subgoal")
		     ],
		     Error_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Analogical plan for ~A in ~A bottoms out"),
				u_target_goal,
				u_target_context
			      ],
			      Roman_nl_Ret47),
	    ElseResult28=[]
	;   get_var(LEnv, u_else, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(LEnv, u_target_context, Target_context_Get),
		f_u_cx_c36_sprout(Target_context_Get, Sprouted_context),
		set_var(LEnv, u_sprouted_context, Sprouted_context),
		f_u_delay_dbgs(u_sprouted_context,
			       
			       [ 
				 [ u_set_ordering,
				   u_sprouted_context,
				   u_ordering
				 ],
				 
				 [ setq,
				   u_bd,
				   
				   [ u_episodic_unify,
				     [u_ob_c36_get, u_rule, [quote, u_goal]],
				     u_r_subgoal_objs,
				     u_source_goal_obj,
				     u_source_subgoals,
				     u_target_goal,
				     u_sprouted_context,
				     u_target_bd,
				     u_belief_path,
				     t,
				     u_top_level_goal,
				     u_analogical_episode
				   ]
				 ],
				 
				 [ if,
				   [u_ob_c63, [car, u_bd]],
				   
				   [ progn,
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule_xtra,
				       '$ARRAY'([*],
						claz_base_character,
						"Resetting target goal from ~A to ~A"),
				       u_target_goal,
				       [car, u_bd]
				     ],
				     [setq, u_target_goal, [car, u_bd]]
				   ]
				 ],
				 
				 [ if,
				   u_reminding_c63,
				   
				   [ progn,
				     
				     [ u_epmem_reminding,
				       u_analogical_episode,
				       [],
				       []
				     ],
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						"Apply episode ~A"),
				       [u_ob_c62_string, u_analogical_episode]
				     ],
				     
				     [ u_rule_fire_msg,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						"analogical plan"),
				       u_target_context,
				       u_bd,
				       u_sprouted_context,
				       u_target_goal
				     ]
				   ],
				   
				   [ progn,
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						"Apply suggested episode ~A"),
				       [u_ob_c62_string, u_analogical_episode]
				     ],
				     
				     [ u_rule_fire_msg,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						"analogical plan"),
				       u_target_context,
				       u_bd,
				       u_sprouted_context,
				       u_target_goal
				     ],
				     [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
				   ]
				 ],
				 
				 [ u_instan_and_activate_subgoals,
				   u_target_goal,
				   u_r_subgoal_objs,
				   u_bd,
				   u_rule,
				   u_sprouted_context,
				   u_seq_c63,
				   u_source_subgoals,
				   [],
				   u_top_level_goal,
				   u_belief_path
				 ]
			       ],
			       Delay_dbgs_Ret),
		get_var(LEnv, u_sprouted_context, Sprouted_context_Get),
		ElseResult28=[Sprouted_context_Get]
	    ;   ElseResult28=[]
	    )
	).
:- set_opv(f_u_run_analogical_plan, classof, claz_function),
   set_opv(u_run_analogical_plan, compile_as, kw_function),
   set_opv(u_run_analogical_plan, function, f_u_run_analogical_plan),
   DefunResult=u_run_analogical_plan.
/*
:- side_effect(assert_lsp(u_run_analogical_plan,
			  wl:lambda_def(defun, u_run_analogical_plan, f_u_run_analogical_plan, [u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], [[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Run analogical plan for ~A in ~A"), u_target_goal, u_target_context], [let_xx, [[u_source_subgoals, [u_goal_subgoals, u_source_goal, u_source_context, u_xx_me_belief_path_xx]], [u_source_goal_obj, [u_ob_c36_get, u_source_goal, [quote, u_obj]]], [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]], [u_bd, []], [u_sprouted_context, []], [u_seq_c63, [u_goal_subgoals_seq_c63, u_source_goal, u_source_context]]], [cond, [[u_null_c63, u_source_subgoals], [error, '$ARRAY'([*], claz_base_character, "I thought bottoming out was detected in activate-subgoal")], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Analogical plan for ~A in ~A bottoms out"), u_target_goal, u_target_context], []], [u_else, [setq, u_sprouted_context, [u_cx_c36_sprout, u_target_context]], [u_delay_dbgs, u_sprouted_context, [u_set_ordering, u_sprouted_context, u_ordering], [setq, u_bd, [u_episodic_unify, [u_ob_c36_get, u_rule, [quote, u_goal]], u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_sprouted_context, u_target_bd, u_belief_path, t, u_top_level_goal, u_analogical_episode]], [if, [u_ob_c63, [car, u_bd]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule_xtra, '$ARRAY'([*], claz_base_character, "Resetting target goal from ~A to ~A"), u_target_goal, [car, u_bd]], [setq, u_target_goal, [car, u_bd]]]], [if, u_reminding_c63, [progn, [u_epmem_reminding, u_analogical_episode, [], []], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Apply episode ~A"), [u_ob_c62_string, u_analogical_episode]], [u_rule_fire_msg, u_rule, '$ARRAY'([*], claz_base_character, "analogical plan"), u_target_context, u_bd, u_sprouted_context, u_target_goal]], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Apply suggested episode ~A"), [u_ob_c62_string, u_analogical_episode]], [u_rule_fire_msg, u_rule, '$ARRAY'([*], claz_base_character, "analogical plan"), u_target_context, u_bd, u_sprouted_context, u_target_goal], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]]], [u_instan_and_activate_subgoals, u_target_goal, u_r_subgoal_objs, u_bd, u_rule, u_sprouted_context, u_seq_c63, u_source_subgoals, [], u_top_level_goal, u_belief_path]], [list, u_sprouted_context]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_run_analogical_plan,
			  wl:arglist_info(u_run_analogical_plan, f_u_run_analogical_plan, [u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], arginfo{all:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], opt:0, req:[u_target_goal, u_target_goal_obj, u_target_context, u_target_bd, u_source_goal, u_source_context, u_rule, u_ordering, u_belief_path, u_top_level_goal, u_analogical_episode, u_reminding_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_run_analogical_plan,
			  wl:init_args(exact_only, f_u_run_analogical_plan))).
*/
/*
 was target context
*/
/*
(defun episodic-unify (rule-goal-obj r-subgoal-objs source-goal-obj
                        source-subgoals target-goal target-context
                        target-bd belief-path disallow-failures?
                        top-level-goal episode)
 (let ((bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd
                                                    belief-path)))
       (seren-ep (if episode (ob$get episode 'seren-ep) nil)))
  (cond
   ((null? bd)
    (if disallow-failures?
        (progn
         (ndbg-roman-nl *gate-dbg* rule ""(defun episodic-unify (rule-goal-obj r-subgoal-objs source-goal-obj\n                        source-subgoals target-goal target-context\n                        target-bd belief-path disallow-failures?\n                        top-level-goal episode)\n (let ((bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd\n                                                    belief-path)))\n       (seren-ep (if episode (ob$get episode 'seren-ep) nil)))\n  (cond\n   ((null? bd)\n    (if disallow-failures?\n        (progn\n         (ndbg-roman-nl *gate-dbg* rule \"~A does not correspond with ~A\"\n                        rule-goal-obj source-goal-obj)\n         (error \"bd nil in analogical plan!!!\")\n         nil)\n        nil))\n   (else\n    ; In following, order has to be not reversed.\n    (yloop\n     (initial (source-subgoalsx source-subgoals))\n     (yfor rule-subgoal-obj in r-subgoal-objs)\n     (yuntil (null? bd))\n     (ydo\n      (setq bd (ob$unify rule-subgoal-obj\n                         (ob$get (car source-subgoalsx) 'obj) bd))\n      (if (and (null? bd) disallow-failures?)\n          (progn\n           (ndbg-roman-nl *gate-dbg* rule \"~A does not correspond with ~A\"\n            rule-subgoal-obj (ob$get (car source-subgoalsx) 'obj))\n           (error \"bd nil in analogical plan!!\")))\n      (setq source-subgoalsx (cdr source-subgoalsx))))\n    (if (null? bd)\n        nil\n        (progn\n         (ndbg-roman-nl *gate-dbg* analogy \"Target-bd:\")\n         (if-interested-in analogy (bd-print target-bd *gate-dbg*))\n         (ndbg-roman-nl *gate-dbg* analogy \"Bd:\")\n         (if-interested-in analogy (bd-print bd *gate-dbg*))\n         (bd-special-append target-bd bd target-goal\n                            target-context belief-path top-level-goal nil\n                            (lambda (x)\n                             (analogy-instantiatible1? x seren-ep)))))))))\n\n; Verification function for reading in episodes.\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:30137 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'episodic-unify',['rule-goal-obj','r-subgoal-objs','source-goal-obj','source-subgoals','target-goal','target-context','target-bd','belief-path','disallow-failures?','top-level-goal',episode],[let,[[bd,['ob$unify','rule-goal-obj','source-goal-obj',['planner-empty-bd','belief-path']]],['seren-ep',[if,episode,['ob$get',episode,[quote,'seren-ep']],[]]]],[cond,[['null?',bd],[if,'disallow-failures?',[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("~A does not correspond with ~A"),'rule-goal-obj','source-goal-obj'],[error,'$STRING'("bd nil in analogical plan!!!")],[]],[]]],[else,[yloop,[initial,['source-subgoalsx','source-subgoals']],[yfor,'rule-subgoal-obj',in,'r-subgoal-objs'],[yuntil,['null?',bd]],[ydo,[setq,bd,['ob$unify','rule-subgoal-obj',['ob$get',[car,'source-subgoalsx'],[quote,obj]],bd]],[if,[and,['null?',bd],'disallow-failures?'],[progn,['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("~A does not correspond with ~A"),'rule-subgoal-obj',['ob$get',[car,'source-subgoalsx'],[quote,obj]]],[error,'$STRING'("bd nil in analogical plan!!")]]],[setq,'source-subgoalsx',[cdr,'source-subgoalsx']]]],[if,['null?',bd],[],[progn,['ndbg-roman-nl','*gate-dbg*',analogy,'$STRING'("Target-bd:")],['if-interested-in',analogy,['bd-print','target-bd','*gate-dbg*']],['ndbg-roman-nl','*gate-dbg*',analogy,'$STRING'("Bd:")],['if-interested-in',analogy,['bd-print',bd,'*gate-dbg*']],['bd-special-append','target-bd',bd,'target-goal','target-context','belief-path','top-level-goal',[],[lambda,[x],['analogy-instantiatible1?',x,'seren-ep']]]]]]]]])
wl:lambda_def(defun, u_episodic_unify, f_u_episodic_unify, [u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], [[let, [[u_bd, [u_ob_c36_unify, u_rule_goal_obj, u_source_goal_obj, [u_planner_empty_bd, u_belief_path]]], [u_seren_ep, [if, u_episode, [u_ob_c36_get, u_episode, [quote, u_seren_ep]], []]]], [cond, [[u_null_c63, u_bd], [if, u_disallow_failures_c63, [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~A does not correspond with ~A"), u_rule_goal_obj, u_source_goal_obj], [error, '$ARRAY'([*], claz_base_character, "bd nil in analogical plan!!!")], []], []]], [u_else, [u_yloop, [u_initial, [u_source_subgoalsx, u_source_subgoals]], [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs], [u_yuntil, [u_null_c63, u_bd]], [u_ydo, [setq, u_bd, [u_ob_c36_unify, u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_bd]], [if, [and, [u_null_c63, u_bd], u_disallow_failures_c63], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~A does not correspond with ~A"), u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]]], [error, '$ARRAY'([*], claz_base_character, "bd nil in analogical plan!!")]]], [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]]], [if, [u_null_c63, u_bd], [], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_analogy, '$ARRAY'([*], claz_base_character, "Target-bd:")], [u_if_interested_in, u_analogy, [u_bd_print, u_target_bd, u_xx_gate_dbg_xx]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_analogy, '$ARRAY'([*], claz_base_character, "Bd:")], [u_if_interested_in, u_analogy, [u_bd_print, u_bd, u_xx_gate_dbg_xx]], [u_bd_special_append, u_target_bd, u_bd, u_target_goal, u_target_context, u_belief_path, u_top_level_goal, [], [lambda, [u_x], [u_analogy_instantiatible1_c63, u_x, u_seren_ep]]]]]]]]]).
wl:arglist_info(u_episodic_unify, f_u_episodic_unify, [u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], arginfo{all:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], opt:0, req:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_episodic_unify).

/*

### Compiled:  `U::EPISODIC-UNIFY` 
*/
f_u_episodic_unify(Rule_goal_obj, R_subgoal_objs, Source_goal_obj, Source_subgoals, Target_goal, Target_context, Target_bd, Belief_path, Disallow_failures_c63, Top_level_goal, Episode, TrueResult36) :-
	nop(global_env(Env)),
	Env42=[bv(u_rule_goal_obj, Rule_goal_obj), bv(u_r_subgoal_objs, R_subgoal_objs), bv(u_source_goal_obj, Source_goal_obj), bv(u_source_subgoals, Source_subgoals), bv(u_target_goal, Target_goal), bv(u_target_context, Target_context), bv(u_target_bd, Target_bd), bv(u_belief_path, Belief_path), bv(u_disallow_failures_c63, Disallow_failures_c63), bv(u_top_level_goal, Top_level_goal), bv(u_episode, Episode)|Env],
	f_u_ob_c36_unify(u_rule_goal_obj,
			 u_source_goal_obj,
			 [u_planner_empty_bd, u_belief_path],
			 Bd_Init),
	get_var(Env42, u_episode, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env42, u_episode, Episode_Get10),
	    f_u_ob_c36_get(Episode_Get10, u_seren_ep, TrueResult),
	    Seren_ep_Init=TrueResult
	;   Seren_ep_Init=[]
	),
	LEnv=[bv(u_bd, Bd_Init), bv(u_seren_ep, Seren_ep_Init)|Env42],
	f_u_null_c63(u_bd, IFTEST14),
	(   IFTEST14\==[]
	->  get_var(LEnv, u_disallow_failures_c63, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "~A does not correspond with ~A"),
				    u_rule_goal_obj,
				    u_source_goal_obj
				  ],
				  Roman_nl_Ret),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "bd nil in analogical plan!!!")
			 ],
			 Error_Ret),
		TrueResult36=[]
	    ;   TrueResult36=[]
	    )
	;   get_var(LEnv, u_else, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_source_subgoalsx, u_source_subgoals]],
			    [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
			    [u_yuntil, [u_null_c63, u_bd]],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_bd,
				
				[ u_ob_c36_unify,
				  u_rule_subgoal_obj,
				  
				  [ u_ob_c36_get,
				    [car, u_source_subgoalsx],
				    [quote, u_obj]
				  ],
				  u_bd
				]
			      ],
			      
			      [ if,
				
				[ and,
				  [u_null_c63, u_bd],
				  u_disallow_failures_c63
				],
				
				[ progn,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_rule,
				    '$ARRAY'([*],
					     claz_base_character,
					     "~A does not correspond with ~A"),
				    u_rule_subgoal_obj,
				    
				    [ u_ob_c36_get,
				      [car, u_source_subgoalsx],
				      [quote, u_obj]
				    ]
				  ],
				  
				  [ error,
				    '$ARRAY'([*],
					     claz_base_character,
					     "bd nil in analogical plan!!")
				  ]
				]
			      ],
			      
			      [ setq,
				u_source_subgoalsx,
				[cdr, u_source_subgoalsx]
			      ]
			    ]
			  ],
			  Yloop_Ret),
		f_u_null_c63(u_bd, IFTEST22),
		(   IFTEST22\==[]
		->  TrueResult36=[]
		;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_analogy,
				      
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "Target-bd:")
				      ],
				      Roman_nl_Ret57),
		    f_u_if_interested_in(u_analogy,
					 
					 [ 
					   [ u_bd_print,
					     u_target_bd,
					     u_xx_gate_dbg_xx
					   ]
					 ],
					 Interested_in_Ret),
		    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_analogy,
				      
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "Bd:")
				      ],
				      Roman_nl_Ret59),
		    f_u_if_interested_in(u_analogy,
					 [[u_bd_print, u_bd, u_xx_gate_dbg_xx]],
					 Interested_in_Ret60),
		    get_var(LEnv, u_bd, Bd_Get),
		    get_var(LEnv, u_belief_path, Belief_path_Get),
		    get_var(LEnv, u_target_bd, Target_bd_Get),
		    get_var(LEnv, u_target_context, Target_context_Get),
		    get_var(LEnv, u_target_goal, Target_goal_Get),
		    get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
		    Lambda=closure([ClosureEnvironment|LEnv], LResult, [u_x],  (get_var(ClosureEnvironment, u_seren_ep, Seren_ep_Get), get_var(ClosureEnvironment, u_x, X_Get), f_u_analogy_instantiatible1_c63(X_Get, Seren_ep_Get, LResult))),
		    f_u_bd_special_append(Target_bd_Get,
					  Bd_Get,
					  Target_goal_Get,
					  Target_context_Get,
					  Belief_path_Get,
					  Top_level_goal_Get,
					  [],
					  Lambda,
					  ElseResult),
		    TrueResult36=ElseResult
		)
	    ;   TrueResult36=[]
	    )
	).
:- set_opv(f_u_episodic_unify, classof, claz_function),
   set_opv(u_episodic_unify, compile_as, kw_function),
   set_opv(u_episodic_unify, function, f_u_episodic_unify),
   DefunResult=u_episodic_unify.
/*
:- side_effect(assert_lsp(u_episodic_unify,
			  wl:lambda_def(defun, u_episodic_unify, f_u_episodic_unify, [u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], [[let, [[u_bd, [u_ob_c36_unify, u_rule_goal_obj, u_source_goal_obj, [u_planner_empty_bd, u_belief_path]]], [u_seren_ep, [if, u_episode, [u_ob_c36_get, u_episode, [quote, u_seren_ep]], []]]], [cond, [[u_null_c63, u_bd], [if, u_disallow_failures_c63, [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~A does not correspond with ~A"), u_rule_goal_obj, u_source_goal_obj], [error, '$ARRAY'([*], claz_base_character, "bd nil in analogical plan!!!")], []], []]], [u_else, [u_yloop, [u_initial, [u_source_subgoalsx, u_source_subgoals]], [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs], [u_yuntil, [u_null_c63, u_bd]], [u_ydo, [setq, u_bd, [u_ob_c36_unify, u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_bd]], [if, [and, [u_null_c63, u_bd], u_disallow_failures_c63], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "~A does not correspond with ~A"), u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]]], [error, '$ARRAY'([*], claz_base_character, "bd nil in analogical plan!!")]]], [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]]], [if, [u_null_c63, u_bd], [], [progn, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_analogy, '$ARRAY'([*], claz_base_character, "Target-bd:")], [u_if_interested_in, u_analogy, [u_bd_print, u_target_bd, u_xx_gate_dbg_xx]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_analogy, '$ARRAY'([*], claz_base_character, "Bd:")], [u_if_interested_in, u_analogy, [u_bd_print, u_bd, u_xx_gate_dbg_xx]], [u_bd_special_append, u_target_bd, u_bd, u_target_goal, u_target_context, u_belief_path, u_top_level_goal, [], [lambda, [u_x], [u_analogy_instantiatible1_c63, u_x, u_seren_ep]]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_episodic_unify,
			  wl:arglist_info(u_episodic_unify, f_u_episodic_unify, [u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], arginfo{all:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], opt:0, req:[u_rule_goal_obj, u_r_subgoal_objs, u_source_goal_obj, u_source_subgoals, u_target_goal, u_target_context, u_target_bd, u_belief_path, u_disallow_failures_c63, u_top_level_goal, u_episode], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_episodic_unify,
			  wl:init_args(exact_only, f_u_episodic_unify))).
*/
/*
 In following, order has to be not reversed.
*/
/*
 Verification function for reading in episodes.
*/
/*
(defun check-episodic-plan (rule source-goal-obj source-subgoals)
  (let* ((rule-goal-obj (ob$get rule 'goal))
         (r-subgoal-objs (rule-subgoal-objs rule))
         (bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd
                                                      *me-belief-path*))))
        (if (null? bd)
            (ndbg-roman-nl *gate-dbg* rule
                           "Warning: "(defun check-episodic-plan (rule source-goal-obj source-subgoals)\n  (let* ((rule-goal-obj (ob$get rule 'goal))\n         (r-subgoal-objs (rule-subgoal-objs rule))\n         (bd (ob$unify rule-goal-obj source-goal-obj (planner-empty-bd\n                                                      *me-belief-path*))))\n        (if (null? bd)\n            (ndbg-roman-nl *gate-dbg* rule\n                           \"Warning: ~A does not correspond with ~A, rule ~A\"\n                           rule-goal-obj source-goal-obj rule)\n            (yloop\n             (initial (source-subgoalsx source-subgoals))\n             (yfor rule-subgoal-obj in r-subgoal-objs)\n             (yuntil (null? bd))\n             (ydo\n              (setq bd (ob$unify rule-subgoal-obj\n                                (ob$get (car source-subgoalsx) 'obj) bd))\n              (if (null? bd)\n               (ndbg-roman-nl *gate-dbg* rule\n                              \"Warning: ~A does not correspond with ~A, rule ~A\"\n                              rule-subgoal-obj (ob$get (car source-subgoalsx)\n                                                       'obj) rule))\n              (setq source-subgoalsx (cdr source-subgoalsx)))))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:32049 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'check-episodic-plan',[rule,'source-goal-obj','source-subgoals'],['let*',[['rule-goal-obj',['ob$get',rule,[quote,goal]]],['r-subgoal-objs',['rule-subgoal-objs',rule]],[bd,['ob$unify','rule-goal-obj','source-goal-obj',['planner-empty-bd','*me-belief-path*']]]],[if,['null?',bd],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Warning: ~A does not correspond with ~A, rule ~A"),'rule-goal-obj','source-goal-obj',rule],[yloop,[initial,['source-subgoalsx','source-subgoals']],[yfor,'rule-subgoal-obj',in,'r-subgoal-objs'],[yuntil,['null?',bd]],[ydo,[setq,bd,['ob$unify','rule-subgoal-obj',['ob$get',[car,'source-subgoalsx'],[quote,obj]],bd]],[if,['null?',bd],['ndbg-roman-nl','*gate-dbg*',rule,'$STRING'("Warning: ~A does not correspond with ~A, rule ~A"),'rule-subgoal-obj',['ob$get',[car,'source-subgoalsx'],[quote,obj]],rule]],[setq,'source-subgoalsx',[cdr,'source-subgoalsx']]]]]]])
wl:lambda_def(defun, u_check_episodic_plan, f_u_check_episodic_plan, [u_rule, u_source_goal_obj, u_source_subgoals], [[let_xx, [[u_rule_goal_obj, [u_ob_c36_get, u_rule, [quote, u_goal]]], [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]], [u_bd, [u_ob_c36_unify, u_rule_goal_obj, u_source_goal_obj, [u_planner_empty_bd, u_xx_me_belief_path_xx]]]], [if, [u_null_c63, u_bd], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Warning: ~A does not correspond with ~A, rule ~A"), u_rule_goal_obj, u_source_goal_obj, u_rule], [u_yloop, [u_initial, [u_source_subgoalsx, u_source_subgoals]], [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs], [u_yuntil, [u_null_c63, u_bd]], [u_ydo, [setq, u_bd, [u_ob_c36_unify, u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_bd]], [if, [u_null_c63, u_bd], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Warning: ~A does not correspond with ~A, rule ~A"), u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_rule]], [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]]]]]]).
wl:arglist_info(u_check_episodic_plan, f_u_check_episodic_plan, [u_rule, u_source_goal_obj, u_source_subgoals], arginfo{all:[u_rule, u_source_goal_obj, u_source_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_source_goal_obj, u_source_subgoals], opt:0, req:[u_rule, u_source_goal_obj, u_source_subgoals], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_check_episodic_plan).

/*

### Compiled:  `U::CHECK-EPISODIC-PLAN` 
*/
f_u_check_episodic_plan(Rule, Source_goal_obj, Source_subgoals, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_rule, Rule), bv(u_source_goal_obj, Source_goal_obj), bv(u_source_subgoals, Source_subgoals)|Env],
	get_var(Env18, u_rule, Rule_Get),
	f_u_ob_c36_get(Rule_Get, u_goal, Rule_goal_obj_Init),
	get_var(Env18, u_rule, Rule_Get8),
	f_u_rule_subgoal_objs(Rule_Get8, R_subgoal_objs_Init),
	f_u_ob_c36_unify(u_rule_goal_obj,
			 u_source_goal_obj,
			 [u_planner_empty_bd, u_xx_me_belief_path_xx],
			 Bd_Init),
	LEnv=[bv(u_rule_goal_obj, Rule_goal_obj_Init), bv(u_r_subgoal_objs, R_subgoal_objs_Init), bv(u_bd, Bd_Init)|Env18],
	f_u_null_c63(u_bd, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Warning: ~A does not correspond with ~A, rule ~A"),
				u_rule_goal_obj,
				u_source_goal_obj,
				u_rule
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ [u_initial, [u_source_subgoalsx, u_source_subgoals]],
			[u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
			[u_yuntil, [u_null_c63, u_bd]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_rule_subgoal_obj,
			      
			      [ u_ob_c36_get,
				[car, u_source_subgoalsx],
				[quote, u_obj]
			      ],
			      u_bd
			    ]
			  ],
			  
			  [ if,
			    [u_null_c63, u_bd],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       "Warning: ~A does not correspond with ~A, rule ~A"),
			      u_rule_subgoal_obj,
			      
			      [ u_ob_c36_get,
				[car, u_source_subgoalsx],
				[quote, u_obj]
			      ],
			      u_rule
			    ]
			  ],
			  [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_check_episodic_plan, classof, claz_function),
   set_opv(u_check_episodic_plan, compile_as, kw_function),
   set_opv(u_check_episodic_plan, function, f_u_check_episodic_plan),
   DefunResult=u_check_episodic_plan.
/*
:- side_effect(assert_lsp(u_check_episodic_plan,
			  wl:lambda_def(defun, u_check_episodic_plan, f_u_check_episodic_plan, [u_rule, u_source_goal_obj, u_source_subgoals], [[let_xx, [[u_rule_goal_obj, [u_ob_c36_get, u_rule, [quote, u_goal]]], [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]], [u_bd, [u_ob_c36_unify, u_rule_goal_obj, u_source_goal_obj, [u_planner_empty_bd, u_xx_me_belief_path_xx]]]], [if, [u_null_c63, u_bd], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Warning: ~A does not correspond with ~A, rule ~A"), u_rule_goal_obj, u_source_goal_obj, u_rule], [u_yloop, [u_initial, [u_source_subgoalsx, u_source_subgoals]], [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs], [u_yuntil, [u_null_c63, u_bd]], [u_ydo, [setq, u_bd, [u_ob_c36_unify, u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_bd]], [if, [u_null_c63, u_bd], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, "Warning: ~A does not correspond with ~A, rule ~A"), u_rule_subgoal_obj, [u_ob_c36_get, [car, u_source_subgoalsx], [quote, u_obj]], u_rule]], [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_check_episodic_plan,
			  wl:arglist_info(u_check_episodic_plan, f_u_check_episodic_plan, [u_rule, u_source_goal_obj, u_source_subgoals], arginfo{all:[u_rule, u_source_goal_obj, u_source_subgoals], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_rule, u_source_goal_obj, u_source_subgoals], opt:0, req:[u_rule, u_source_goal_obj, u_source_subgoals], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_check_episodic_plan,
			  wl:init_args(exact_only, f_u_check_episodic_plan))).
*/
/*
(defun generate-episode (episode)
  (generate1 episode *global-switches* (ob$get episode 'context)
             *me-belief-path*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33240 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'generate-episode',[episode],[generate1,episode,'*global-switches*',['ob$get',episode,[quote,context]],'*me-belief-path*']])
wl:lambda_def(defun, u_generate_episode, f_u_generate_episode, [u_episode], [[u_generate1, u_episode, u_xx_global_switches_xx, [u_ob_c36_get, u_episode, [quote, u_context]], u_xx_me_belief_path_xx]]).
wl:arglist_info(u_generate_episode, f_u_generate_episode, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_generate_episode).

/*

### Compiled:  `U::GENERATE-EPISODE` 
*/
f_u_generate_episode(Episode, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_episode, Episode)|Env],
	get_var(Env10, u_episode, Episode_Get6),
	get_var(Env10, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
	f_u_ob_c36_get(Episode_Get6, u_context, Context),
	get_var(Env10, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_generate1(Episode_Get6,
		      Xx_global_switches_xx_Get,
		      Context,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_generate_episode, classof, claz_function),
   set_opv(u_generate_episode, compile_as, kw_function),
   set_opv(u_generate_episode, function, f_u_generate_episode),
   DefunResult=u_generate_episode.
/*
:- side_effect(assert_lsp(u_generate_episode,
			  wl:lambda_def(defun, u_generate_episode, f_u_generate_episode, [u_episode], [[u_generate1, u_episode, u_xx_global_switches_xx, [u_ob_c36_get, u_episode, [quote, u_context]], u_xx_me_belief_path_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_generate_episode,
			  wl:arglist_info(u_generate_episode, f_u_generate_episode, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_generate_episode,
			  wl:init_args(exact_only, f_u_generate_episode))).
*/
/*
(defun task-print-plans (tlg)
  (yloop (initial (continue? t))
        (yfor leaf in (cx$leaf-descendants (get-backtrack-wall tlg)))
        (ywhile continue?)
        (ydo (format *gate-dbg* "Leaf context "(defun task-print-plans (tlg)\n  (yloop (initial (continue? t))\n        (yfor leaf in (cx$leaf-descendants (get-backtrack-wall tlg)))\n        (ywhile continue?)\n        (ydo (format *gate-dbg* \"Leaf context ~A~%\" leaf)\n            (plan-print tlg leaf)\n;            (setq continue? (interrogate \"More? \"))\n            (setq continue? t))\n        (yresult nil)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33372 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'task-print-plans',[tlg],[yloop,[initial,['continue?',t]],[yfor,leaf,in,['cx$leaf-descendants',['get-backtrack-wall',tlg]]],[ywhile,'continue?'],[ydo,[format,'*gate-dbg*','$STRING'("Leaf context ~A~%"),leaf],['plan-print',tlg,leaf],[setq,'continue?',t]],[yresult,[]]]])
wl:lambda_def(defun, u_task_print_plans, f_u_task_print_plans, [u_tlg], [[u_yloop, [u_initial, [u_continue_c63, t]], [u_yfor, u_leaf, u_in, [u_cx_c36_leaf_descendants, [u_get_backtrack_wall, u_tlg]]], [u_ywhile, u_continue_c63], [u_ydo, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "Leaf context ~A~%"), u_leaf], [u_plan_print, u_tlg, u_leaf], [setq, u_continue_c63, t]], [u_yresult, []]]]).
wl:arglist_info(u_task_print_plans, f_u_task_print_plans, [u_tlg], arginfo{all:[u_tlg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_tlg], opt:0, req:[u_tlg], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_task_print_plans).

/*

### Compiled:  `U::TASK-PRINT-PLANS` 
*/
f_u_task_print_plans(Tlg, FnResult) :-
	nop(global_env(Env)),
	Env6=[bv(u_tlg, Tlg)|Env],
	f_u_yloop(
		  [ [u_initial, [u_continue_c63, t]],
		    
		    [ u_yfor,
		      u_leaf,
		      u_in,
		      [u_cx_c36_leaf_descendants, [u_get_backtrack_wall, u_tlg]]
		    ],
		    [u_ywhile, u_continue_c63],
		    
		    [ u_ydo,
		      
		      [ format,
			u_xx_gate_dbg_xx,
			'$ARRAY'([*], claz_base_character, "Leaf context ~A~%"),
			u_leaf
		      ],
		      [u_plan_print, u_tlg, u_leaf],
		      [setq, u_continue_c63, t]
		    ],
		    [u_yresult, []]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_task_print_plans, classof, claz_function),
   set_opv(u_task_print_plans, compile_as, kw_function),
   set_opv(u_task_print_plans, function, f_u_task_print_plans),
   DefunResult=u_task_print_plans.
/*
:- side_effect(assert_lsp(u_task_print_plans,
			  wl:lambda_def(defun, u_task_print_plans, f_u_task_print_plans, [u_tlg], [[u_yloop, [u_initial, [u_continue_c63, t]], [u_yfor, u_leaf, u_in, [u_cx_c36_leaf_descendants, [u_get_backtrack_wall, u_tlg]]], [u_ywhile, u_continue_c63], [u_ydo, [format, u_xx_gate_dbg_xx, '$ARRAY'([*], claz_base_character, "Leaf context ~A~%"), u_leaf], [u_plan_print, u_tlg, u_leaf], [setq, u_continue_c63, t]], [u_yresult, []]]]))).
*/
/*
:- side_effect(assert_lsp(u_task_print_plans,
			  wl:arglist_info(u_task_print_plans, f_u_task_print_plans, [u_tlg], arginfo{all:[u_tlg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_tlg], opt:0, req:[u_tlg], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_task_print_plans,
			  wl:init_args(exact_only, f_u_task_print_plans))).
*/
/*
            (setq continue? (interrogate "More? "))
*/
/*
(defun ep-print (episode)
  (plan-print1 (ob$get episode 'goal) (ob$get episode 'context) *gate-dbg* 0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33734 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ep-print',[episode],['plan-print1',['ob$get',episode,[quote,goal]],['ob$get',episode,[quote,context]],'*gate-dbg*',0]])
wl:lambda_def(defun, u_ep_print, f_u_ep_print, [u_episode], [[u_plan_print1, [u_ob_c36_get, u_episode, [quote, u_goal]], [u_ob_c36_get, u_episode, [quote, u_context]], u_xx_gate_dbg_xx, 0]]).
wl:arglist_info(u_ep_print, f_u_ep_print, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ep_print).

/*

### Compiled:  `U::EP-PRINT` 
*/
f_u_ep_print(Episode, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_episode, Episode)|Env],
	get_var(Env9, u_episode, Episode_Get),
	f_u_ob_c36_get(Episode_Get, u_goal, Goal),
	get_var(Env9, u_episode, Episode_Get5),
	f_u_ob_c36_get(Episode_Get5, u_context, Context),
	get_var(Env9, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	f_u_plan_print1(Goal, Context, Xx_gate_dbg_xx_Get, 0, Plan_print1_Ret),
	Plan_print1_Ret=FnResult.
:- set_opv(f_u_ep_print, classof, claz_function),
   set_opv(u_ep_print, compile_as, kw_function),
   set_opv(u_ep_print, function, f_u_ep_print),
   DefunResult=u_ep_print.
/*
:- side_effect(assert_lsp(u_ep_print,
			  wl:lambda_def(defun, u_ep_print, f_u_ep_print, [u_episode], [[u_plan_print1, [u_ob_c36_get, u_episode, [quote, u_goal]], [u_ob_c36_get, u_episode, [quote, u_context]], u_xx_gate_dbg_xx, 0]]))).
*/
/*
:- side_effect(assert_lsp(u_ep_print,
			  wl:arglist_info(u_ep_print, f_u_ep_print, [u_episode], arginfo{all:[u_episode], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_episode], opt:0, req:[u_episode], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ep_print, wl:init_args(exact_only, f_u_ep_print))).
*/
/*
(defun plan-print (goal context)
  (plan-print1 goal context *gate-dbg* 0))

; There is probably no such thing as always-prop
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33840 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'plan-print',[goal,context],['plan-print1',goal,context,'*gate-dbg*',0]])
wl:lambda_def(defun, u_plan_print, f_u_plan_print, [u_goal, u_context], [[u_plan_print1, u_goal, u_context, u_xx_gate_dbg_xx, 0]]).
wl:arglist_info(u_plan_print, f_u_plan_print, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_plan_print).

/*

### Compiled:  `U::PLAN-PRINT` 
*/
f_u_plan_print(Goal, Context, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_goal, Goal), bv(u_context, Context)|Env],
	get_var(Env9, u_context, Context_Get),
	get_var(Env9, u_goal, Goal_Get),
	get_var(Env9, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	f_u_plan_print1(Goal_Get,
			Context_Get,
			Xx_gate_dbg_xx_Get,
			0,
			Plan_print1_Ret),
	Plan_print1_Ret=FnResult.
:- set_opv(f_u_plan_print, classof, claz_function),
   set_opv(u_plan_print, compile_as, kw_function),
   set_opv(u_plan_print, function, f_u_plan_print),
   DefunResult=u_plan_print.
/*
:- side_effect(assert_lsp(u_plan_print,
			  wl:lambda_def(defun, u_plan_print, f_u_plan_print, [u_goal, u_context], [[u_plan_print1, u_goal, u_context, u_xx_gate_dbg_xx, 0]]))).
*/
/*
:- side_effect(assert_lsp(u_plan_print,
			  wl:arglist_info(u_plan_print, f_u_plan_print, [u_goal, u_context], arginfo{all:[u_goal, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context], opt:0, req:[u_goal, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_plan_print,
			  wl:init_args(exact_only, f_u_plan_print))).
*/
/*
 There is probably no such thing as always-prop
*/
/*
(setq *ep-print-options* '(parens always-prop no-newline))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33966 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*ep-print-options*',[quote,[parens,'always-prop','no-newline']]])
:- set_var(AEnv,
	   setq,
	   u_xx_ep_print_options_xx,
	   [u_parens, u_always_prop, u_no_newline]).
/*
(defun plan-print1 (goal context stream level)
  (print-spaces stream (* level 2))
  (cond
   ((ty$instance? goal 'active-goal)
    (if *typeset?*
        (format stream "["(defun plan-print1 (goal context stream level)\n  (print-spaces stream (* level 2))\n  (cond\n   ((ty$instance? goal 'active-goal)\n    (if *typeset?*\n        (format stream \"[~A: (\\\\typepp{AG}. \" (ob->string goal))\n        (format stream \"[~A: (AG. \" (ob->string goal))))\n   ((ty$instance? goal 'failed-goal)\n    (if *typeset?*\n        (format stream \"[~A: (\\\\typepp{FG}. \" (ob->string goal))\n        (format stream \"[~A: (FG. \" (ob->string goal))))\n   ((ty$instance? goal 'succeeded-goal)\n    (if *typeset?*\n        (format stream \"[~A: (\\\\typepp{SG}. \" (ob->string goal))\n        (format stream \"[~A: (SG. \" (ob->string goal))))\n   (else\n    (format stream \"[~A: (?? \" (ob->string goal))))\n  (if (ob$get goal 'obj)\n      (ob$pr (ob$get goal 'obj) stream *ep-print-options*)\n      (format stream \"--\"))\n  (if (ob$get goal 'episode)\n      (format stream \") ~A]\" (ob->string (ob$get goal 'episode)))\n      (format stream \")]\"))\n  (do-newline stream)\n  (yloop (yfor subgoal in (goal-subgoals goal context *me-belief-path*))\n         (ydo (plan-print1 subgoal context stream (+ 1 level)))))\n\n; ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:34026 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'plan-print1',[goal,context,stream,level],['print-spaces',stream,[*,level,2]],[cond,[['ty$instance?',goal,[quote,'active-goal']],[if,'*typeset?*',[format,stream,'$STRING'("[~A: (\\typepp{AG}. "),['ob->string',goal]],[format,stream,'$STRING'("[~A: (AG. "),['ob->string',goal]]]],[['ty$instance?',goal,[quote,'failed-goal']],[if,'*typeset?*',[format,stream,'$STRING'("[~A: (\\typepp{FG}. "),['ob->string',goal]],[format,stream,'$STRING'("[~A: (FG. "),['ob->string',goal]]]],[['ty$instance?',goal,[quote,'succeeded-goal']],[if,'*typeset?*',[format,stream,'$STRING'("[~A: (\\typepp{SG}. "),['ob->string',goal]],[format,stream,'$STRING'("[~A: (SG. "),['ob->string',goal]]]],[else,[format,stream,'$STRING'("[~A: (?? "),['ob->string',goal]]]],[if,['ob$get',goal,[quote,obj]],['ob$pr',['ob$get',goal,[quote,obj]],stream,'*ep-print-options*'],[format,stream,'$STRING'("--")]],[if,['ob$get',goal,[quote,episode]],[format,stream,'$STRING'(") ~A]"),['ob->string',['ob$get',goal,[quote,episode]]]],[format,stream,'$STRING'(")]")]],['do-newline',stream],[yloop,[yfor,subgoal,in,['goal-subgoals',goal,context,'*me-belief-path*']],[ydo,['plan-print1',subgoal,context,stream,[+,1,level]]]]])
wl:lambda_def(defun, u_plan_print1, f_u_plan_print1, [u_goal, u_context, stream, u_level], [[u_print_spaces, stream, [*, u_level, 2]], [cond, [[u_ty_c36_instance_c63, u_goal, [quote, u_active_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{AG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (AG. "), [u_ob_c62_string, u_goal]]]], [[u_ty_c36_instance_c63, u_goal, [quote, u_failed_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{FG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (FG. "), [u_ob_c62_string, u_goal]]]], [[u_ty_c36_instance_c63, u_goal, [quote, u_succeeded_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{SG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (SG. "), [u_ob_c62_string, u_goal]]]], [u_else, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (?? "), [u_ob_c62_string, u_goal]]]], [if, [u_ob_c36_get, u_goal, [quote, u_obj]], [u_ob_c36_pr, [u_ob_c36_get, u_goal, [quote, u_obj]], stream, u_xx_ep_print_options_xx], [format, stream, '$ARRAY'([*], claz_base_character, "--")]], [if, [u_ob_c36_get, u_goal, [quote, u_episode]], [format, stream, '$ARRAY'([*], claz_base_character, ") ~A]"), [u_ob_c62_string, [u_ob_c36_get, u_goal, [quote, u_episode]]]], [format, stream, '$ARRAY'([*], claz_base_character, ")]")]], [u_do_newline, stream], [u_yloop, [u_yfor, u_subgoal, u_in, [u_goal_subgoals, u_goal, u_context, u_xx_me_belief_path_xx]], [u_ydo, [u_plan_print1, u_subgoal, u_context, stream, [+, 1, u_level]]]]]).
wl:arglist_info(u_plan_print1, f_u_plan_print1, [u_goal, u_context, stream, u_level], arginfo{all:[u_goal, u_context, stream, u_level], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, stream, u_level], opt:0, req:[u_goal, u_context, stream, u_level], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_plan_print1).

/*

### Compiled:  `U::PLAN-PRINT1` 
*/
f_u_plan_print1(Goal, Context, Stream, Level, FnResult) :-
	nop(global_env(Env)),
	Env75=[bv(u_goal, Goal), bv(u_context, Context), bv(stream, Stream), bv(u_level, Level)|Env],
	get_var(Env75, stream, Stream_Get),
	get_var(Env75, u_level, Level_Get),
	*(Level_Get, 2, _305754440),
	f_u_print_spaces(Stream_Get, _305754440, Print_spaces_Ret),
	get_var(Env75, u_goal, Goal_Get),
	f_u_ty_c36_instance_c63(Goal_Get, u_active_goal, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env75, u_xx_typeset_c63_xx, IFTEST9),
	    (   IFTEST9\==[]
	    ->  get_var(Env75, stream, Stream_Get12),
		get_var(Env75, u_goal, Goal_Get13),
		f_u_ob_c62_string(Goal_Get13, C62_string_Ret),
		cl_format(
			  [ Stream_Get12,
			    '$ARRAY'([*],
				     claz_base_character,
				     "[~A: (\\typepp{AG}. "),
			    C62_string_Ret
			  ],
			  TrueResult),
		TrueResult49=TrueResult
	    ;   get_var(Env75, stream, Stream_Get14),
		get_var(Env75, u_goal, Goal_Get15),
		f_u_ob_c62_string(Goal_Get15, C62_string_Ret84),
		cl_format(
			  [ Stream_Get14,
			    '$ARRAY'([*], claz_base_character, "[~A: (AG. "),
			    C62_string_Ret84
			  ],
			  ElseResult),
		TrueResult49=ElseResult
	    )
	;   get_var(Env75, u_goal, Goal_Get20),
	    f_u_ty_c36_instance_c63(Goal_Get20, u_failed_goal, IFTEST18),
	    (   IFTEST18\==[]
	    ->  get_var(Env75, u_xx_typeset_c63_xx, IFTEST21),
		(   IFTEST21\==[]
		->  get_var(Env75, stream, Stream_Get24),
		    get_var(Env75, u_goal, Goal_Get25),
		    f_u_ob_c62_string(Goal_Get25, C62_string_Ret85),
		    cl_format(
			      [ Stream_Get24,
				'$ARRAY'([*],
					 claz_base_character,
					 "[~A: (\\typepp{FG}. "),
				C62_string_Ret85
			      ],
			      TrueResult28),
		    TrueResult49=TrueResult28
		;   get_var(Env75, stream, Stream_Get26),
		    get_var(Env75, u_goal, Goal_Get27),
		    f_u_ob_c62_string(Goal_Get27, C62_string_Ret86),
		    cl_format(
			      [ Stream_Get26,
				'$ARRAY'([*], claz_base_character, "[~A: (FG. "),
				C62_string_Ret86
			      ],
			      ElseResult29),
		    TrueResult49=ElseResult29
		)
	    ;   get_var(Env75, u_goal, Goal_Get32),
		f_u_ty_c36_instance_c63(Goal_Get32, u_succeeded_goal, IFTEST30),
		(   IFTEST30\==[]
		->  get_var(Env75, u_xx_typeset_c63_xx, IFTEST33),
		    (   IFTEST33\==[]
		    ->  get_var(Env75, stream, Stream_Get36),
			get_var(Env75, u_goal, Goal_Get37),
			f_u_ob_c62_string(Goal_Get37, C62_string_Ret87),
			cl_format(
				  [ Stream_Get36,
				    '$ARRAY'([*],
					     claz_base_character,
					     "[~A: (\\typepp{SG}. "),
				    C62_string_Ret87
				  ],
				  TrueResult40),
			TrueResult49=TrueResult40
		    ;   get_var(Env75, stream, Stream_Get38),
			get_var(Env75, u_goal, Goal_Get39),
			f_u_ob_c62_string(Goal_Get39, C62_string_Ret88),
			cl_format(
				  [ Stream_Get38,
				    '$ARRAY'([*],
					     claz_base_character,
					     "[~A: (SG. "),
				    C62_string_Ret88
				  ],
				  ElseResult41),
			TrueResult49=ElseResult41
		    )
		;   get_var(Env75, u_else, IFTEST42),
		    (   IFTEST42\==[]
		    ->  get_var(Env75, stream, Stream_Get45),
			get_var(Env75, u_goal, Goal_Get46),
			f_u_ob_c62_string(Goal_Get46, C62_string_Ret89),
			cl_format(
				  [ Stream_Get45,
				    '$ARRAY'([*],
					     claz_base_character,
					     "[~A: (?? "),
				    C62_string_Ret89
				  ],
				  TrueResult47),
			TrueResult49=TrueResult47
		    ;   TrueResult49=[]
		    )
		)
	    )
	),
	get_var(Env75, u_goal, Goal_Get57),
	f_u_ob_c36_get(Goal_Get57, u_obj, IFTEST55),
	(   IFTEST55\==[]
	->  get_var(Env75, u_goal, Goal_Get58),
	    f_u_ob_c36_get(Goal_Get58, u_obj, Obj),
	    get_var(Env75, stream, Stream_Get59),
	    get_var(Env75, u_xx_ep_print_options_xx, Xx_ep_print_options_xx_Get),
	    f_u_ob_c36_pr(Obj,
			  Stream_Get59,
			  Xx_ep_print_options_xx_Get,
			  TrueResult62),
	    _306062506=TrueResult62
	;   get_var(Env75, stream, Stream_Get61),
	    cl_format([Stream_Get61, '$ARRAY'([*], claz_base_character, "--")],
		      ElseResult63),
	    _306062506=ElseResult63
	),
	get_var(Env75, u_goal, Goal_Get66),
	f_u_ob_c36_get(Goal_Get66, u_episode, IFTEST64),
	(   IFTEST64\==[]
	->  get_var(Env75, stream, Stream_Get67),
	    get_var(Env75, u_goal, Goal_Get68),
	    f_u_ob_c36_get(Goal_Get68, u_episode, Episode),
	    f_u_ob_c62_string(Episode, C62_string_Ret90),
	    cl_format(
		      [ Stream_Get67,
			'$ARRAY'([*], claz_base_character, ") ~A]"),
			C62_string_Ret90
		      ],
		      TrueResult70),
	    _306076358=TrueResult70
	;   get_var(Env75, stream, Stream_Get69),
	    cl_format([Stream_Get69, '$ARRAY'([*], claz_base_character, ")]")],
		      ElseResult71),
	    _306076358=ElseResult71
	),
	get_var(Env75, stream, Stream_Get72),
	f_u_do_newline(Stream_Get72, Do_newline_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_subgoal,
		      u_in,
		      
		      [ u_goal_subgoals,
			u_goal,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ],
		    
		    [ u_ydo,
		      [u_plan_print1, u_subgoal, u_context, stream, [+, 1, u_level]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_plan_print1, classof, claz_function),
   set_opv(u_plan_print1, compile_as, kw_function),
   set_opv(u_plan_print1, function, f_u_plan_print1),
   DefunResult=u_plan_print1.
/*
:- side_effect(assert_lsp(u_plan_print1,
			  wl:lambda_def(defun, u_plan_print1, f_u_plan_print1, [u_goal, u_context, stream, u_level], [[u_print_spaces, stream, [*, u_level, 2]], [cond, [[u_ty_c36_instance_c63, u_goal, [quote, u_active_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{AG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (AG. "), [u_ob_c62_string, u_goal]]]], [[u_ty_c36_instance_c63, u_goal, [quote, u_failed_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{FG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (FG. "), [u_ob_c62_string, u_goal]]]], [[u_ty_c36_instance_c63, u_goal, [quote, u_succeeded_goal]], [if, u_xx_typeset_c63_xx, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (\\typepp{SG}. "), [u_ob_c62_string, u_goal]], [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (SG. "), [u_ob_c62_string, u_goal]]]], [u_else, [format, stream, '$ARRAY'([*], claz_base_character, "[~A: (?? "), [u_ob_c62_string, u_goal]]]], [if, [u_ob_c36_get, u_goal, [quote, u_obj]], [u_ob_c36_pr, [u_ob_c36_get, u_goal, [quote, u_obj]], stream, u_xx_ep_print_options_xx], [format, stream, '$ARRAY'([*], claz_base_character, "--")]], [if, [u_ob_c36_get, u_goal, [quote, u_episode]], [format, stream, '$ARRAY'([*], claz_base_character, ") ~A]"), [u_ob_c62_string, [u_ob_c36_get, u_goal, [quote, u_episode]]]], [format, stream, '$ARRAY'([*], claz_base_character, ")]")]], [u_do_newline, stream], [u_yloop, [u_yfor, u_subgoal, u_in, [u_goal_subgoals, u_goal, u_context, u_xx_me_belief_path_xx]], [u_ydo, [u_plan_print1, u_subgoal, u_context, stream, [+, 1, u_level]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_plan_print1,
			  wl:arglist_info(u_plan_print1, f_u_plan_print1, [u_goal, u_context, stream, u_level], arginfo{all:[u_goal, u_context, stream, u_level], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_goal, u_context, stream, u_level], opt:0, req:[u_goal, u_context, stream, u_level], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_plan_print1,
			  wl:init_args(exact_only, f_u_plan_print1))).
*/
/*
 End of file.
*/


%; Total compilation time: 34.819 seconds


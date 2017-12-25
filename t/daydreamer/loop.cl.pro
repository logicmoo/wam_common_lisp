#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "loop" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:24 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;; CHANGES
*/
/*
;; 19990429: Made this not a package. (erik)
*/
/*
;; 19990506: I notice this is quite buggy: (1) if the YUNTIL clause contains
*/
/*
;; the YFOR loop variable, it takes on the value NIL, (2) the YUNTIL clause
*/
/*
;; cannot be placed before the YFOR clause. (erik)
*/
/*
;; Version 1.1 released.
*/
/*
;; Tidied up test code, comments cleaned up a bit, provide yloop as feature
*/
/*
;;   and package, added version number, fixed ydo to be safer, included
*/
/*
;;   hints from Jeni Tennison.  5-16-94 -FER
*/
/*
;; fixed restrictive yfor i from x to y where x and y were required to be
*/
/*
;;   numebrs at load time -fr 2/5/88
*/
/*
;; made all clause keywords local to yloop package, and updated documentation
*/
/*
;;    to reflect this fact.  -fr 10/27/86
*/
/*
;; end tests also checked in the beginning to catch the case of no iteration
*/
/*
;;     needed -fr 10/08/86
*/
/*
;; note that I optimized the code in for so that it didn't eval its arg's 
*/
/*
;;    twice -fr 7/19/86
*/
/*
;; multiple end tests accomidated by use of or on end-tests
*/
/*
;;   -fr 9/10/86
*/
/*
;; Code originially in  conger:>jim>yale-loop>yale-loop.lisp. 
*/
/*
;; Also available via anonymous ftp from 
*/
/*
;;  ftp.cs.cmu.edu as 
*/
/*
;;    /afs/cs/user/mkant/Public/Lisp/code/iter/loop/yloop/yloop.cl
*/
/*
;;  and unicorn.ccc.nottingham.ac.uk as
*/
/*
;;    pub/lpzfr/yloop.l
*/
/*
;;          
*/
/*
;; Questions or requests for later versions:
*/
/*
;; Jim Panagos (jpanagos@world.std.com) or 
*/
/*
;; Frank.Ritter@nottingham.ac.uk  (or Ritter@cs.cmu.edu) 
*/
/*
;;
*/
/*
;; 
*/
/*
;; takes the following keywords:
*/
/*
;;      YLOOP
*/
/*
;;      INITIAL    (INITIAL (var1 val1) (var2 val2) ...)
*/
/*
;;      BEFORE     (BEFORE (to-do-1) (to-do-2) ...)
*/
/*
;;      AFTER      (AFTER  (to-do-1)(to-do-2) ...)
*/
/*
;;      YFOR       (YFOR var1 {IN ON FROM} var2 TO var3)
*/
/*
;;                    IN gets cars of the list, ON gets the cdrs
*/
/*
;;      YDO        (YDO (to-do-1) (to-do-2) ...)
*/
/*
;;      YRESULT    returns the rest of the clause in an implicet progn, or nil
*/
/*
;;      NEXT       (NEXT   (var1 (+ var1 1)))
*/
/*
;;      YWHILE     (YWHILE  {var1 (test)} )
*/
/*
;;      YUNTIL     (YUNTIL  {var1 (test)} )
*/
/*
;;      INCR
*/
/*
;;      DECR
*/
/*
;;      MAXIMIZE
*/
/*
;;      SUM
*/
/*
;;      YWHEN
*/
/*
;;      LERROR
*/
/*
;;
*/
/*
;; Yale loop macro written in Common Lisp based on the loop construct in 
*/
/*
;; McDermont, Charniak and Riesbeck's AI programming book.
*/
/*
;;
*/
/*
;; DESIGN APPROACH
*/
/*
;; Each loop statement such as before or ywhile are macros themselves. Given
*/
/*
;; their arguments they fill out a template of what should be stored in
*/
/*
;; appropriate positions in the loop. The loop macro then fetches the
*/
/*
;; collection of all templates and pieces them together the loop code (all 
*/
/*
;; templates stored in the global *loop-alist*). The advantage of this 
*/
/*
;; approach is that the syntax of the loop is order independent.
*/
/*
;
*/
/*
;; LOCAL LOOP VARIABLES
*/
/*
;;
*/
/*
;; Use INITIAL to define variables within the scope of the loop.
*/
/*
;; E.g. (initial (foo 5) (bar 'baz)). This will be translated in 
*/
/*
;; (let* ( (foo 5) (bar 'baz) ..) ..). Notice that 
*/
/*
;; bindings are done sequentially via let*
*/
/*
;;
*/
/*
;; ITERATION DRIVING CLAUSES
*/
/*
;;
*/
/*
;; The iteration driving clauses are those discussed the sections of
*/
/*
;; numeric iteration
*/
/*
;; mapping of lists and the macros YWHILE and YUNTIL.  (YWHILE x) and (YUNTIL y)
*/
/*
;; are translated to expand into (if (not x) nil (go loop)) and (if y nil 
*/
/*
;; (go loop))
*/
/*
;; 
*/
/*
;; NUMERIC ITERATION
*/
/*
;; There are 2 ways to perform numeric iteration. The first is via the YFOR 
*/
/*
;; statement:
*/
/*
;; e.g. (YFOR iteration-variable FROM begin-iteration TO  end-iteration STEP
*/
/*
;;  inc-or-dec) [downward stepping can be implemented using negative steps]
*/
/*
;; The second is via the (incr ..) and (decr ..) constructs. FROM and IN are 
*/
/*
;; synonyms in this construct. If the .IN. type construct is desired 
*/
/*
;; (see documentation), use IN not FROM. A step value is optional in both
*/
/*
;; cases and defaults to 1.
*/
/*
;;
*/
/*
;; MAPPING OF LISTS
*/
/*
;; Two constructs are provided for list mapping both accessible via the FOR
*/
/*
;; statement. The IN construct permits mapping over successive elements of 
*/
/*
;; the list. Eg. (yfor a in '(1 2 3 4))
*/
/*
;; The ON constuct is similar to in except that it maps over successive cdrs.
*/
/*
;;
*/
/*
;; Examples 
*/
/*
;;
*/
/*
;; (yloop (incr a .in. 0 to 10) (ydo (print a))) ;print 0..10
*/
/*
;; (yloop (ydo (print 'a)))
*/
/*
;; 
*/
/*
;; (yloop(initially a 0 b 5)(yfor x from 0 to 10)(ydo(print x))(yresult b))
*/
/*
;;    will print 0..10 and return 5.
*/
/*
;;
*/
/*
;; 
*/
/*
;;
*/
/*
;; ADDING NEW LOOP MACROS
*/
/*
;;
*/
/*
;; Code has been provided to add the user define his/her own loop macros. 
*/
/*
;; See explanation and code in the file.
*/
/*
;;
*/
/*
;; HINTS
*/
/*
;;
*/
/*
;; On Translation time syntax checking: as clauses are independent macros, 
*/
/*
;; translation time syntax checking will be clumbersome. The  values in 
*/
/*
;; *loop-alist* will have to be used after that list is fully constituted.
*/
/*
;;
*/
/*
;; EXPORT CONTROL
*/
/*
;;
*/
/*
;; Note that all symbols that will be used in trio, or some other package, 
*/
/*
;; have to be exported.
*/
/*
;  ;; fix 9-May-93 -FER  in DEFINE-AND-RENAME-LOOP-LOCALS NIL
*/
/*
;  (if result (add-element-to-loop-alist (cons 'progn (list result)) 'result))
*/
/*
;
*/
/*
; Also need to fix yloop so that results get spliced in within a prog, list,
*/
/*
;  or values (don't know how result is being used if there are multiple ones,  
*/
/*
(defvar *loop-alist* () "To contain translated loop information ")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5209 **********************/
:-lisp_compile_to_prolog(pkg_user,[defvar,'*loop-alist*',[],'$STRING'("To contain translated loop information ")])
doc: doc_string(u_xx_loop_alist_xx,
	      pkg_user,
	      variable,
	      "To contain translated loop information ").

:- set_var(AEnv, defvar, u_xx_loop_alist_xx, []).
/*
:- side_effect(assert_lsp(u_xx_loop_alist_xx,
			  doc:doc_string(u_xx_loop_alist_xx, pkg_user, variable, "To contain translated loop information "))).
*/
/*
(defmacro clear-loop-alist ()
  `(setf *loop-alist* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5278 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'clear-loop-alist',[],['#BQ',[setf,'*loop-alist*',[]]]])
wl:lambda_def(defmacro, u_clear_loop_alist, f_u_clear_loop_alist, [], [progn, ['#BQ', [setf, u_xx_loop_alist_xx, []]]]).
wl:arglist_info(u_clear_loop_alist, f_u_clear_loop_alist, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_clear_loop_alist).

/*

### Compiled:  `U::CLEAR-LOOP-ALIST` 
*/
f_u_clear_loop_alist(FnResult) :-
	global_env(Global_env_Ret),
	_51691836=Global_env_Ret,
	[setf, u_xx_loop_alist_xx, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_clear_loop_alist, classof, claz_macro),
   set_opv(u_clear_loop_alist, compile_as, kw_operator),
   set_opv(u_clear_loop_alist, function, f_u_clear_loop_alist),
   _Ignored4=u_clear_loop_alist.
/*
:- side_effect(assert_lsp(u_clear_loop_alist,
			  wl:lambda_def(defmacro, u_clear_loop_alist, f_u_clear_loop_alist, [], [progn, ['#BQ', [setf, u_xx_loop_alist_xx, []]]]))).
*/
/*
:- side_effect(assert_lsp(u_clear_loop_alist,
			  wl:arglist_info(u_clear_loop_alist, f_u_clear_loop_alist, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_clear_loop_alist,
			  wl:init_args(exact_only, f_u_clear_loop_alist))).
*/
/*
(defmacro fetch-clauses (clause-key)
  `(car (rassoc ',clause-key *loop-alist*)))
        
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5337 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fetch-clauses',['clause-key'],['#BQ',[car,[rassoc,[quote,['#COMMA','clause-key']],'*loop-alist*']]]])
wl:lambda_def(defmacro, u_fetch_clauses, f_u_fetch_clauses, [u_clause_key], [progn, ['#BQ', [car, [rassoc, [quote, ['#COMMA', u_clause_key]], u_xx_loop_alist_xx]]]]).
wl:arglist_info(u_fetch_clauses, f_u_fetch_clauses, [u_clause_key], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_clause_key], opt:0, req:[u_clause_key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fetch_clauses).

/*

### Compiled:  `U::FETCH-CLAUSES` 
*/
f_u_fetch_clauses(Clause_key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_clause_key, Clause_key)|Global_env_Ret],
	get_var(Env, u_clause_key, Clause_key_Get),
	[car, [rassoc, [quote, Clause_key_Get], u_xx_loop_alist_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fetch_clauses, classof, claz_macro),
   set_opv(u_fetch_clauses, compile_as, kw_operator),
   set_opv(u_fetch_clauses, function, f_u_fetch_clauses),
   _Ignored4=u_fetch_clauses.
/*
:- side_effect(assert_lsp(u_fetch_clauses,
			  wl:lambda_def(defmacro, u_fetch_clauses, f_u_fetch_clauses, [u_clause_key], [progn, ['#BQ', [car, [rassoc, [quote, ['#COMMA', u_clause_key]], u_xx_loop_alist_xx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fetch_clauses,
			  wl:arglist_info(u_fetch_clauses, f_u_fetch_clauses, [u_clause_key], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_clause_key], opt:0, req:[u_clause_key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fetch_clauses,
			  wl:init_args(exact_only, f_u_fetch_clauses))).
*/
/*
(defmacro acons-setf (key datum list)  
  "Functions like acons accept changes list to the new value."
  `(setf ,list (acons ,key ,datum ,list)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5428 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'acons-setf',[key,datum,list],'$STRING'("Functions like acons accept changes list to the new value."),['#BQ',[setf,['#COMMA',list],[acons,['#COMMA',key],['#COMMA',datum],['#COMMA',list]]]]])
doc: doc_string(u_acons_setf,
	      _52759768,
	      function,
	      "Functions like acons accept changes list to the new value.").

wl:lambda_def(defmacro, u_acons_setf, f_u_acons_setf, [key, u_datum, list], [progn, ['#BQ', [setf, ['#COMMA', list], [acons, ['#COMMA', key], ['#COMMA', u_datum], ['#COMMA', list]]]]]).
wl:arglist_info(u_acons_setf, f_u_acons_setf, [key, u_datum, list], arginfo{all:[key, u_datum, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key, u_datum, list], opt:0, req:[key, u_datum, list], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_acons_setf).

/*

### Compiled:  `U::ACONS-SETF` 
*/
f_u_acons_setf(Key, Datum, List, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(key, Key), bv(u_datum, Datum), bv(list, List)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, list, List_Get10),
	get_var(Env, u_datum, Datum_Get),
	[setf, List_Get10, [acons, Key_Get, Datum_Get, List_Get10]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_acons_setf, classof, claz_macro),
   set_opv(u_acons_setf, compile_as, kw_operator),
   set_opv(u_acons_setf, function, f_u_acons_setf),
   _Ignored4=u_acons_setf.
/*
:- side_effect(assert_lsp(u_acons_setf,
			  doc:doc_string(u_acons_setf, _52759768, function, "Functions like acons accept changes list to the new value."))).
*/
/*
:- side_effect(assert_lsp(u_acons_setf,
			  wl:lambda_def(defmacro, u_acons_setf, f_u_acons_setf, [key, u_datum, list], [progn, ['#BQ', [setf, ['#COMMA', list], [acons, ['#COMMA', key], ['#COMMA', u_datum], ['#COMMA', list]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_acons_setf,
			  wl:arglist_info(u_acons_setf, f_u_acons_setf, [key, u_datum, list], arginfo{all:[key, u_datum, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key, u_datum, list], opt:0, req:[key, u_datum, list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_acons_setf,
			  wl:init_args(exact_only, f_u_acons_setf))).
*/
/*
(defmacro before (&rest body)  
  `(add-element-to-loop-alist (cons 'progn ',body) 'before))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5575 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,before,['&rest',body],['#BQ',['add-element-to-loop-alist',[cons,[quote,progn],[quote,['#COMMA',body]]],[quote,before]]]])
wl:lambda_def(defmacro, u_before, f_u_before, [c38_rest, u_body], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_body]]], [quote, u_before]]]]).
wl:arglist_info(u_before, f_u_before, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}).
wl: init_args(0, f_u_before).

/*

### Compiled:  `U::BEFORE` 
*/
f_u_before(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_body, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_body, Body_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Body_Get]], [quote, u_before]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_before, classof, claz_macro),
   set_opv(u_before, compile_as, kw_operator),
   set_opv(u_before, function, f_u_before),
   _Ignored4=u_before.
/*
:- side_effect(assert_lsp(u_before,
			  wl:lambda_def(defmacro, u_before, f_u_before, [c38_rest, u_body], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_body]]], [quote, u_before]]]]))).
*/
/*
:- side_effect(assert_lsp(u_before,
			  wl:arglist_info(u_before, f_u_before, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_before, wl:init_args(0, f_u_before))).
*/
/*
(defmacro initial (&rest body)
  `(dolist (clause ',body)
    (add-element-to-loop-alist clause 'initializations))) 

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5669 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,initial,['&rest',body],['#BQ',[dolist,[clause,[quote,['#COMMA',body]]],['add-element-to-loop-alist',clause,[quote,initializations]]]]])
wl:lambda_def(defmacro, u_initial, f_u_initial, [c38_rest, u_body], [progn, ['#BQ', [dolist, [u_clause, [quote, ['#COMMA', u_body]]], [u_add_element_to_loop_alist, u_clause, [quote, u_initializations]]]]]).
wl:arglist_info(u_initial, f_u_initial, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}).
wl: init_args(0, f_u_initial).

/*

### Compiled:  `U::INITIAL` 
*/
f_u_initial(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_body, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_body, Body_Get),
	[dolist, [u_clause, [quote, Body_Get]], [u_add_element_to_loop_alist, u_clause, [quote, u_initializations]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_initial, classof, claz_macro),
   set_opv(u_initial, compile_as, kw_operator),
   set_opv(u_initial, function, f_u_initial),
   _Ignored4=u_initial.
/*
:- side_effect(assert_lsp(u_initial,
			  wl:lambda_def(defmacro, u_initial, f_u_initial, [c38_rest, u_body], [progn, ['#BQ', [dolist, [u_clause, [quote, ['#COMMA', u_body]]], [u_add_element_to_loop_alist, u_clause, [quote, u_initializations]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_initial,
			  wl:arglist_info(u_initial, f_u_initial, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_initial, wl:init_args(0, f_u_initial))).
*/
/*
(defmacro next (&rest clauses)
  "Next clauses must be of the form (var exp). Eg (next (i (+ i 1)))."
  `(let ( (assignment-list nil)        )
     (dolist (clause ',clauses)
        (setf assignment-list (cons (cons 'setf clause) assignment-list)))
     (add-element-to-loop-alist (cons 'progn assignment-list) 'next)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:5787 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,next,['&rest',clauses],'$STRING'("Next clauses must be of the form (var exp). Eg (next (i (+ i 1)))."),['#BQ',[let,[['assignment-list',[]]],[dolist,[clause,[quote,['#COMMA',clauses]]],[setf,'assignment-list',[cons,[cons,[quote,setf],clause],'assignment-list']]],['add-element-to-loop-alist',[cons,[quote,progn],'assignment-list'],[quote,next]]]]])
doc: doc_string(u_next,
	      _54844248,
	      function,
	      "Next clauses must be of the form (var exp). Eg (next (i (+ i 1))).").

wl:lambda_def(defmacro, u_next, f_u_next, [c38_rest, u_clauses], [progn, ['#BQ', [let, [[u_assignment_list, []]], [dolist, [u_clause, [quote, ['#COMMA', u_clauses]]], [setf, u_assignment_list, [cons, [cons, [quote, setf], u_clause], u_assignment_list]]], [u_add_element_to_loop_alist, [cons, [quote, progn], u_assignment_list], [quote, u_next]]]]]).
wl:arglist_info(u_next, f_u_next, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}).
wl: init_args(0, f_u_next).

/*

### Compiled:  `U::NEXT` 
*/
f_u_next(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_clauses, Clauses_Get),
	[let, [[u_assignment_list, []]], [dolist, [u_clause, [quote, Clauses_Get]], [setf, u_assignment_list, [cons, [cons, [quote, setf], u_clause], u_assignment_list]]], [u_add_element_to_loop_alist, [cons, [quote, progn], u_assignment_list], [quote, u_next]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_next, classof, claz_macro),
   set_opv(u_next, compile_as, kw_operator),
   set_opv(u_next, function, f_u_next),
   _Ignored4=u_next.
/*
:- side_effect(assert_lsp(u_next,
			  doc:doc_string(u_next, _54844248, function, "Next clauses must be of the form (var exp). Eg (next (i (+ i 1)))."))).
*/
/*
:- side_effect(assert_lsp(u_next,
			  wl:lambda_def(defmacro, u_next, f_u_next, [c38_rest, u_clauses], [progn, ['#BQ', [let, [[u_assignment_list, []]], [dolist, [u_clause, [quote, ['#COMMA', u_clauses]]], [setf, u_assignment_list, [cons, [cons, [quote, setf], u_clause], u_assignment_list]]], [u_add_element_to_loop_alist, [cons, [quote, progn], u_assignment_list], [quote, u_next]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_next,
			  wl:arglist_info(u_next, f_u_next, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_next, wl:init_args(0, f_u_next))).
*/
/*
(defmacro yresult (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'result))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6109 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,yresult,['&rest',clauses],['#BQ',['add-element-to-loop-alist',[cons,[quote,progn],[quote,['#COMMA',clauses]]],[quote,result]]]])
wl:lambda_def(defmacro, u_yresult, f_u_yresult, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, u_result]]]]).
wl:arglist_info(u_yresult, f_u_yresult, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}).
wl: init_args(0, f_u_yresult).

/*

### Compiled:  `U::YRESULT` 
*/
f_u_yresult(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, u_result]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yresult, classof, claz_macro),
   set_opv(u_yresult, compile_as, kw_operator),
   set_opv(u_yresult, function, f_u_yresult),
   _Ignored4=u_yresult.
/*
:- side_effect(assert_lsp(u_yresult,
			  wl:lambda_def(defmacro, u_yresult, f_u_yresult, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_yresult,
			  wl:arglist_info(u_yresult, f_u_yresult, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_yresult, wl:init_args(0, f_u_yresult))).
*/
/*
(defmacro ydo (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'do))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6208 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,ydo,['&rest',clauses],['#BQ',['add-element-to-loop-alist',[cons,[quote,progn],[quote,['#COMMA',clauses]]],[quote,do]]]])
wl:lambda_def(defmacro, u_ydo, f_u_ydo, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, do]]]]).
wl:arglist_info(u_ydo, f_u_ydo, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}).
wl: init_args(0, f_u_ydo).

/*

### Compiled:  `U::YDO` 
*/
f_u_ydo(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, do]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ydo, classof, claz_macro),
   set_opv(u_ydo, compile_as, kw_operator),
   set_opv(u_ydo, function, f_u_ydo),
   _Ignored4=u_ydo.
/*
:- side_effect(assert_lsp(u_ydo,
			  wl:lambda_def(defmacro, u_ydo, f_u_ydo, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, do]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ydo,
			  wl:arglist_info(u_ydo, f_u_ydo, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ydo, wl:init_args(0, f_u_ydo))).
*/
/*
(defmacro ywhile (expression)
  `(add-element-to-loop-alist (list 'not ',expression) 'end-test))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6299 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,ywhile,[expression],['#BQ',['add-element-to-loop-alist',[list,[quote,not],[quote,['#COMMA',expression]]],[quote,'end-test']]]])
wl:lambda_def(defmacro, u_ywhile, f_u_ywhile, [u_expression], [progn, ['#BQ', [u_add_element_to_loop_alist, [list, [quote, not], [quote, ['#COMMA', u_expression]]], [quote, u_end_test]]]]).
wl:arglist_info(u_ywhile, f_u_ywhile, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ywhile).

/*

### Compiled:  `U::YWHILE` 
*/
f_u_ywhile(Expression, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_expression, Expression)|Global_env_Ret],
	get_var(Env, u_expression, Expression_Get),
	[u_add_element_to_loop_alist, [list, [quote, not], [quote, Expression_Get]], [quote, u_end_test]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ywhile, classof, claz_macro),
   set_opv(u_ywhile, compile_as, kw_operator),
   set_opv(u_ywhile, function, f_u_ywhile),
   _Ignored4=u_ywhile.
/*
:- side_effect(assert_lsp(u_ywhile,
			  wl:lambda_def(defmacro, u_ywhile, f_u_ywhile, [u_expression], [progn, ['#BQ', [u_add_element_to_loop_alist, [list, [quote, not], [quote, ['#COMMA', u_expression]]], [quote, u_end_test]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ywhile,
			  wl:arglist_info(u_ywhile, f_u_ywhile, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ywhile, wl:init_args(exact_only, f_u_ywhile))).
*/
/*
(defmacro yuntil (expression)
  `(add-element-to-loop-alist  ',expression 'end-test))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6397 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,yuntil,[expression],['#BQ',['add-element-to-loop-alist',[quote,['#COMMA',expression]],[quote,'end-test']]]])
wl:lambda_def(defmacro, u_yuntil, f_u_yuntil, [u_expression], [progn, ['#BQ', [u_add_element_to_loop_alist, [quote, ['#COMMA', u_expression]], [quote, u_end_test]]]]).
wl:arglist_info(u_yuntil, f_u_yuntil, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_yuntil).

/*

### Compiled:  `U::YUNTIL` 
*/
f_u_yuntil(Expression, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_expression, Expression)|Global_env_Ret],
	get_var(Env, u_expression, Expression_Get),
	[u_add_element_to_loop_alist, [quote, Expression_Get], [quote, u_end_test]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yuntil, classof, claz_macro),
   set_opv(u_yuntil, compile_as, kw_operator),
   set_opv(u_yuntil, function, f_u_yuntil),
   _Ignored4=u_yuntil.
/*
:- side_effect(assert_lsp(u_yuntil,
			  wl:lambda_def(defmacro, u_yuntil, f_u_yuntil, [u_expression], [progn, ['#BQ', [u_add_element_to_loop_alist, [quote, ['#COMMA', u_expression]], [quote, u_end_test]]]]))).
*/
/*
:- side_effect(assert_lsp(u_yuntil,
			  wl:arglist_info(u_yuntil, f_u_yuntil, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_yuntil, wl:init_args(exact_only, f_u_yuntil))).
*/
/*
(defmacro lerror (format-string &rest format-args)
  `(error ,format-string ,@format-args))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6484 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,lerror,['format-string','&rest','format-args'],['#BQ',[error,['#COMMA','format-string'],['#BQ-COMMA-ELIPSE','format-args']]]])
wl:lambda_def(defmacro, u_lerror, f_u_lerror, [u_format_string, c38_rest, u_format_args], [progn, ['#BQ', [error, ['#COMMA', u_format_string], ['#BQ-COMMA-ELIPSE', u_format_args]]]]).
wl:arglist_info(u_lerror, f_u_lerror, [u_format_string, c38_rest, u_format_args], arginfo{all:[u_format_string], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_format_string, u_format_args], opt:0, req:[u_format_string], rest:[u_format_args], sublists:0, whole:0}).
wl: init_args(1, f_u_lerror).

/*

### Compiled:  `U::LERROR` 
*/
f_u_lerror(Format_string, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_format_string, Format_string), bv(u_format_args, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_format_args, Format_args_Get),
	get_var(GEnv, u_format_string, Format_string_Get),
	[error, Format_string_Get|Format_args_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_lerror, classof, claz_macro),
   set_opv(u_lerror, compile_as, kw_operator),
   set_opv(u_lerror, function, f_u_lerror),
   _Ignored4=u_lerror.
/*
:- side_effect(assert_lsp(u_lerror,
			  wl:lambda_def(defmacro, u_lerror, f_u_lerror, [u_format_string, c38_rest, u_format_args], [progn, ['#BQ', [error, ['#COMMA', u_format_string], ['#BQ-COMMA-ELIPSE', u_format_args]]]]))).
*/
/*
:- side_effect(assert_lsp(u_lerror,
			  wl:arglist_info(u_lerror, f_u_lerror, [u_format_string, c38_rest, u_format_args], arginfo{all:[u_format_string], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_format_string, u_format_args], opt:0, req:[u_format_string], rest:[u_format_args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_lerror, wl:init_args(1, f_u_lerror))).
*/
/*
(defvar *stepping-variable* nil 
  "Dummy variable to nest macros without compiler barf.")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6577 **********************/
:-lisp_compile_to_prolog(pkg_user,[defvar,'*stepping-variable*',[],'$STRING'("Dummy variable to nest macros without compiler barf.")])
doc: doc_string(u_xx_stepping_variable_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(AEnv, defvar, u_xx_stepping_variable_xx, []).
/*
:- side_effect(assert_lsp(u_xx_stepping_variable_xx,
			  doc:doc_string(u_xx_stepping_variable_xx, pkg_user, variable, "Dummy variable to nest macros without compiler barf."))).
*/
/*
(defvar *what-to-do* nil 
  "Dummy variable to nest macros without compiler barf.")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6668 **********************/
:-lisp_compile_to_prolog(pkg_user,[defvar,'*what-to-do*',[],'$STRING'("Dummy variable to nest macros without compiler barf.")])
doc: doc_string(u_xx_what_to_do_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(AEnv, defvar, u_xx_what_to_do_xx, []).
/*
:- side_effect(assert_lsp(u_xx_what_to_do_xx,
			  doc:doc_string(u_xx_what_to_do_xx, pkg_user, variable, "Dummy variable to nest macros without compiler barf."))).
*/
/*
(defvar *llist* nil 
  "Dummy variable to nest macros without compiler barf.")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6752 **********************/
:-lisp_compile_to_prolog(pkg_user,[defvar,'*llist*',[],'$STRING'("Dummy variable to nest macros without compiler barf.")])
doc: doc_string(u_xx_llist_xx,
	      pkg_user,
	      variable,
	      "Dummy variable to nest macros without compiler barf.").

:- set_var(AEnv, defvar, u_xx_llist_xx, []).
/*
:- side_effect(assert_lsp(u_xx_llist_xx,
			  doc:doc_string(u_xx_llist_xx, pkg_user, variable, "Dummy variable to nest macros without compiler barf."))).
*/
/*
(defmacro yfor  (variable what-to-do &rest llist)
  (let ((iteration-variable (gensym))
          (iteration-expression (gensym))
          stepping-variable)
    `(let ((,iteration-variable nil)
           (,iteration-expression nil) )
       ,(record-in-loop-alist `(,variable ,iteration-variable) 'iteration-variable)
       #+ignore(format t ""(defmacro yfor  (variable what-to-do &rest llist)\n  (let ((iteration-variable (gensym))\n          (iteration-expression (gensym))\n          stepping-variable)\n    `(let ((,iteration-variable nil)\n           (,iteration-expression nil) )\n       ,(record-in-loop-alist `(,variable ,iteration-variable) 'iteration-variable)\n       #+ignore(format t \"~% yfor variable is: ~a ~% and it is ~a to in\"\n                       (intern (symbol-name what-to-do))\n                (eq 'in (intern (symbol-name what-to-do))))\n       ,(case (intern (symbol-name what-to-do))\n          (in\n            (record-in-loop-alist `(endp ,iteration-expression) 'end-test)\n            (add-elements-to-clause 'next\n            `(setf ,iteration-variable (car ,iteration-expression))\n            `(setf ,iteration-expression (cdr ,iteration-expression)))\n            (add-elements-to-clause 'initializations\n                                    `(,iteration-variable  ;(car ,@llist))\n                                                           (car ,iteration-expression))\n                                    `(,iteration-expression  ,@llist))\n            )   \n          (on\n            (record-in-loop-alist iteration-expression 'iteration-control-variable)\n            (record-in-loop-alist `(endp ,iteration-expression) 'end-test)\n            (add-elements-to-clause 'next\n                             `(setf ,iteration-variable ,iteration-expression)\n                             `(setf ,iteration-expression (cdr ,iteration-expression)))\n            ; note that since you are in a let*, don't eval the expression twice, use\n            ; the variable that it will be bound to\n            (add-elements-to-clause 'initializations\n                             `(,iteration-variable  (car ,iteration-expression))\n                            `(,iteration-expression ,@llist)))\n          (from     \n            (if (null (fifth llist)) (setf stepping-variable 1)\n                  (setf stepping-variable (fifth llist)))\n            (cond\n              ((> (length llist) 5) \n                (lerror \"YL:Too many clauses in (yfor ~a ~a ..)\" variable\n                                       what-to-do))\n              ((and (minusp stepping-variable)(<= (first llist) (third llist)))\n               (lerror \"YL:Cannot decrement from ~a to ~a\" \n                        (first llist) (third llist)))\n              (t \n               (add-element-to-loop-alist `(,iteration-variable ,(first llist))\n                                          'initializations)\n               (add-element-to-loop-alist `(setf ,iteration-variable\n                                            (+ ,iteration-variable ,stepping-variable)) 'next)\n               (if (minusp stepping-variable )\n                   (add-element-to-loop-alist `(< ,iteration-variable ,(third llist))\n                                              'end-test )\n                   (add-element-to-loop-alist `(> ,iteration-variable ,(third llist))\n                                              'end-test)))))\n       ))) t)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:6832 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,yfor,[variable,'what-to-do','&rest',llist],[let,[['iteration-variable',[gensym]],['iteration-expression',[gensym]],'stepping-variable'],['#BQ',[let,[[['#COMMA','iteration-variable'],[]],[['#COMMA','iteration-expression'],[]]],['#COMMA',['record-in-loop-alist',['#BQ',[['#COMMA',variable],['#COMMA','iteration-variable']]],[quote,'iteration-variable']]],['#COMMA',[case,[intern,['symbol-name','what-to-do']],[in,['record-in-loop-alist',['#BQ',[endp,['#COMMA','iteration-expression']]],[quote,'end-test']],['add-elements-to-clause',[quote,next],['#BQ',[setf,['#COMMA','iteration-variable'],[car,['#COMMA','iteration-expression']]]],['#BQ',[setf,['#COMMA','iteration-expression'],[cdr,['#COMMA','iteration-expression']]]]],['add-elements-to-clause',[quote,initializations],['#BQ',[['#COMMA','iteration-variable'],[car,['#COMMA','iteration-expression']]]],['#BQ',[['#COMMA','iteration-expression'],['#BQ-COMMA-ELIPSE',llist]]]]],[on,['record-in-loop-alist','iteration-expression',[quote,'iteration-control-variable']],['record-in-loop-alist',['#BQ',[endp,['#COMMA','iteration-expression']]],[quote,'end-test']],['add-elements-to-clause',[quote,next],['#BQ',[setf,['#COMMA','iteration-variable'],['#COMMA','iteration-expression']]],['#BQ',[setf,['#COMMA','iteration-expression'],[cdr,['#COMMA','iteration-expression']]]]],['add-elements-to-clause',[quote,initializations],['#BQ',[['#COMMA','iteration-variable'],[car,['#COMMA','iteration-expression']]]],['#BQ',[['#COMMA','iteration-expression'],['#BQ-COMMA-ELIPSE',llist]]]]],[from,[if,[null,[fifth,llist]],[setf,'stepping-variable',1],[setf,'stepping-variable',[fifth,llist]]],[cond,[[>,[length,llist],5],[lerror,'$STRING'("YL:Too many clauses in (yfor ~a ~a ..)"),variable,'what-to-do']],[[and,[minusp,'stepping-variable'],[<=,[first,llist],[third,llist]]],[lerror,'$STRING'("YL:Cannot decrement from ~a to ~a"),[first,llist],[third,llist]]],[t,['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',[first,llist]]]],[quote,initializations]],['add-element-to-loop-alist',['#BQ',[setf,['#COMMA','iteration-variable'],[+,['#COMMA','iteration-variable'],['#COMMA','stepping-variable']]]],[quote,next]],[if,[minusp,'stepping-variable'],['add-element-to-loop-alist',['#BQ',[<,['#COMMA','iteration-variable'],['#COMMA',[third,llist]]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[>,['#COMMA','iteration-variable'],['#COMMA',[third,llist]]]],[quote,'end-test']]]]]]]]]]],t])
/*
% case:-[[u_in,[u_record_in_loop_alist,['#BQ',[endp,['#COMMA',u_iteration_expression]]],[quote,u_end_test]],[u_add_elements_to_clause,[quote,u_next],['#BQ',[setf,['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[setf,['#COMMA',u_iteration_expression],[cdr,['#COMMA',u_iteration_expression]]]]],[u_add_elements_to_clause,[quote,u_initializations],['#BQ',[['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[['#COMMA',u_iteration_expression],['#BQ-COMMA-ELIPSE',u_llist]]]]],[u_on,[u_record_in_loop_alist,u_iteration_expression,[quote,u_iteration_control_variable]],[u_record_in_loop_alist,['#BQ',[endp,['#COMMA',u_iteration_expression]]],[quote,u_end_test]],[u_add_elements_to_clause,[quote,u_next],['#BQ',[setf,['#COMMA',u_iteration_variable],['#COMMA',u_iteration_expression]]],['#BQ',[setf,['#COMMA',u_iteration_expression],[cdr,['#COMMA',u_iteration_expression]]]]],[u_add_elements_to_clause,[quote,u_initializations],['#BQ',[['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[['#COMMA',u_iteration_expression],['#BQ-COMMA-ELIPSE',u_llist]]]]],[u_from,[if,[null,[fifth,u_llist]],[setf,u_stepping_variable,1],[setf,u_stepping_variable,[fifth,u_llist]]],[cond,[[>,[length,u_llist],5],[u_lerror,'$ARRAY'([*],claz_base_character,"YL:Too many clauses in (yfor ~a ~a ..)"),variable,u_what_to_do]],[[and,[minusp,u_stepping_variable],[<=,[first,u_llist],[third,u_llist]]],[u_lerror,'$ARRAY'([*],claz_base_character,"YL:Cannot decrement from ~a to ~a"),[first,u_llist],[third,u_llist]]],[t,[u_add_element_to_loop_alist,['#BQ',[['#COMMA',u_iteration_variable],['#COMMA',[first,u_llist]]]],[quote,u_initializations]],[u_add_element_to_loop_alist,['#BQ',[setf,['#COMMA',u_iteration_variable],[+,['#COMMA',u_iteration_variable],['#COMMA',u_stepping_variable]]]],[quote,u_next]],[if,[minusp,u_stepping_variable],[u_add_element_to_loop_alist,['#BQ',[<,['#COMMA',u_iteration_variable],['#COMMA',[third,u_llist]]]],[quote,u_end_test]],[u_add_element_to_loop_alist,['#BQ',[>,['#COMMA',u_iteration_variable],['#COMMA',[third,u_llist]]]],[quote,u_end_test]]]]]]].
*/
/*
% conds:-[[[eq,_59668408,[quote,u_in]],[progn,[u_record_in_loop_alist,['#BQ',[endp,['#COMMA',u_iteration_expression]]],[quote,u_end_test]],[u_add_elements_to_clause,[quote,u_next],['#BQ',[setf,['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[setf,['#COMMA',u_iteration_expression],[cdr,['#COMMA',u_iteration_expression]]]]],[u_add_elements_to_clause,[quote,u_initializations],['#BQ',[['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[['#COMMA',u_iteration_expression],['#BQ-COMMA-ELIPSE',u_llist]]]]]],[[eq,_59668408,[quote,u_on]],[progn,[u_record_in_loop_alist,u_iteration_expression,[quote,u_iteration_control_variable]],[u_record_in_loop_alist,['#BQ',[endp,['#COMMA',u_iteration_expression]]],[quote,u_end_test]],[u_add_elements_to_clause,[quote,u_next],['#BQ',[setf,['#COMMA',u_iteration_variable],['#COMMA',u_iteration_expression]]],['#BQ',[setf,['#COMMA',u_iteration_expression],[cdr,['#COMMA',u_iteration_expression]]]]],[u_add_elements_to_clause,[quote,u_initializations],['#BQ',[['#COMMA',u_iteration_variable],[car,['#COMMA',u_iteration_expression]]]],['#BQ',[['#COMMA',u_iteration_expression],['#BQ-COMMA-ELIPSE',u_llist]]]]]],[[eq,_59668408,[quote,u_from]],[progn,[if,[null,[fifth,u_llist]],[setf,u_stepping_variable,1],[setf,u_stepping_variable,[fifth,u_llist]]],[cond,[[>,[length,u_llist],5],[u_lerror,'$ARRAY'([*],claz_base_character,"YL:Too many clauses in (yfor ~a ~a ..)"),variable,u_what_to_do]],[[and,[minusp,u_stepping_variable],[<=,[first,u_llist],[third,u_llist]]],[u_lerror,'$ARRAY'([*],claz_base_character,"YL:Cannot decrement from ~a to ~a"),[first,u_llist],[third,u_llist]]],[t,[u_add_element_to_loop_alist,['#BQ',[['#COMMA',u_iteration_variable],['#COMMA',[first,u_llist]]]],[quote,u_initializations]],[u_add_element_to_loop_alist,['#BQ',[setf,['#COMMA',u_iteration_variable],[+,['#COMMA',u_iteration_variable],['#COMMA',u_stepping_variable]]]],[quote,u_next]],[if,[minusp,u_stepping_variable],[u_add_element_to_loop_alist,['#BQ',[<,['#COMMA',u_iteration_variable],['#COMMA',[third,u_llist]]]],[quote,u_end_test]],[u_add_element_to_loop_alist,['#BQ',[>,['#COMMA',u_iteration_variable],['#COMMA',[third,u_llist]]]],[quote,u_end_test]]]]]]]].
*/
wl:lambda_def(defmacro, u_yfor, f_u_yfor, [variable, u_what_to_do, c38_rest, u_llist], [progn, [let, [[u_iteration_variable, [gensym]], [u_iteration_expression, [gensym]], u_stepping_variable], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []], [['#COMMA', u_iteration_expression], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [case, [intern, [symbol_name, u_what_to_do]], [u_in, [u_record_in_loop_alist, ['#BQ', [endp, ['#COMMA', u_iteration_expression]]], [quote, u_end_test]], [u_add_elements_to_clause, [quote, u_next], ['#BQ', [setf, ['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [setf, ['#COMMA', u_iteration_expression], [cdr, ['#COMMA', u_iteration_expression]]]]], [u_add_elements_to_clause, [quote, u_initializations], ['#BQ', [['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [['#COMMA', u_iteration_expression], ['#BQ-COMMA-ELIPSE', u_llist]]]]], [u_on, [u_record_in_loop_alist, u_iteration_expression, [quote, u_iteration_control_variable]], [u_record_in_loop_alist, ['#BQ', [endp, ['#COMMA', u_iteration_expression]]], [quote, u_end_test]], [u_add_elements_to_clause, [quote, u_next], ['#BQ', [setf, ['#COMMA', u_iteration_variable], ['#COMMA', u_iteration_expression]]], ['#BQ', [setf, ['#COMMA', u_iteration_expression], [cdr, ['#COMMA', u_iteration_expression]]]]], [u_add_elements_to_clause, [quote, u_initializations], ['#BQ', [['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [['#COMMA', u_iteration_expression], ['#BQ-COMMA-ELIPSE', u_llist]]]]], [u_from, [if, [null, [fifth, u_llist]], [setf, u_stepping_variable, 1], [setf, u_stepping_variable, [fifth, u_llist]]], [cond, [[>, [length, u_llist], 5], [u_lerror, '$ARRAY'([*], claz_base_character, "YL:Too many clauses in (yfor ~a ~a ..)"), variable, u_what_to_do]], [[and, [minusp, u_stepping_variable], [<=, [first, u_llist], [third, u_llist]]], [u_lerror, '$ARRAY'([*], claz_base_character, "YL:Cannot decrement from ~a to ~a"), [first, u_llist], [third, u_llist]]], [t, [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', [first, u_llist]]]], [quote, u_initializations]], [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [+, ['#COMMA', u_iteration_variable], ['#COMMA', u_stepping_variable]]]], [quote, u_next]], [if, [minusp, u_stepping_variable], [u_add_element_to_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', [third, u_llist]]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', [third, u_llist]]]], [quote, u_end_test]]]]]]]]]]], t]).
wl:arglist_info(u_yfor, f_u_yfor, [variable, u_what_to_do, c38_rest, u_llist], arginfo{all:[variable, u_what_to_do], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_what_to_do, u_llist], opt:0, req:[variable, u_what_to_do], rest:[u_llist], sublists:0, whole:0}).
wl: init_args(2, f_u_yfor).

/*

### Compiled:  `U::YFOR` 
*/
f_u_yfor(Variable, What_to_do, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(variable, Variable), bv(u_what_to_do, What_to_do), bv(u_llist, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Iteration_variable_Init),
	cl_gensym(Iteration_expression_Init),
	LEnv=[bv(u_iteration_variable, Iteration_variable_Init), bv(u_iteration_expression, Iteration_expression_Init), bv(u_stepping_variable, [])|CDR],
	get_var(LEnv, u_iteration_expression, Iteration_expression_Get),
	get_var(LEnv, u_iteration_variable, Iteration_variable_Get16),
	get_var(LEnv, variable, Variable_Get),
	f_u_record_in_loop_alist([Variable_Get, Iteration_variable_Get16],
				 u_iteration_variable,
				 Iteration_variable),
	get_var(LEnv, u_what_to_do, What_to_do_Get),
	cl_symbol_name(What_to_do_Get, Intern_Param),
	cl_intern(Intern_Param, Key),
	(   is_eq(Key, u_in)
	->  get_var(LEnv, u_iteration_expression, Iteration_expression_Get22),
	    f_u_record_in_loop_alist([endp, Iteration_expression_Get22],
				     u_end_test,
				     End_test),
	    get_var(LEnv, u_iteration_expression, Iteration_expression_Get24),
	    get_var(LEnv, u_iteration_variable, Iteration_variable_Get23),
	    f_u_add_elements_to_clause(u_next,
				       
				       [ setf,
					 Iteration_variable_Get23,
					 [car, Iteration_expression_Get24]
				       ],
				       
				       [ setf,
					 Iteration_expression_Get24,
					 [cdr, Iteration_expression_Get24]
				       ],
				       To_clause_Ret),
	    get_var(LEnv, u_iteration_expression, Iteration_expression_Get28),
	    get_var(LEnv, u_iteration_variable, Iteration_variable_Get27),
	    get_var(LEnv, u_llist, Llist_Get),
	    f_u_add_elements_to_clause(u_initializations,
				       
				       [ Iteration_variable_Get27,
					 [car, Iteration_expression_Get28]
				       ],
				       [Iteration_expression_Get28|Llist_Get],
				       TrueResult86),
	    ElseResult79=TrueResult86
	;   is_eq(Key, u_on)
	->  get_var(LEnv, u_iteration_expression, Iteration_expression_Get33),
	    f_u_record_in_loop_alist(Iteration_expression_Get33,
				     u_iteration_control_variable,
				     Iteration_control_variable),
	    get_var(LEnv, u_iteration_expression, Iteration_expression_Get34),
	    f_u_record_in_loop_alist([endp, Iteration_expression_Get34],
				     u_end_test,
				     End_test94),
	    get_var(LEnv, u_iteration_expression, Iteration_expression_Get36),
	    get_var(LEnv, u_iteration_variable, Iteration_variable_Get35),
	    f_u_add_elements_to_clause(u_next,
				       
				       [ setf,
					 Iteration_variable_Get35,
					 Iteration_expression_Get36
				       ],
				       
				       [ setf,
					 Iteration_expression_Get36,
					 [cdr, Iteration_expression_Get36]
				       ],
				       To_clause_Ret101),
	    get_var(LEnv, u_iteration_expression, Iteration_expression_Get40),
	    get_var(LEnv, u_iteration_variable, Iteration_variable_Get39),
	    get_var(LEnv, u_llist, Llist_Get42),
	    f_u_add_elements_to_clause(u_initializations,
				       
				       [ Iteration_variable_Get39,
					 [car, Iteration_expression_Get40]
				       ],
				       
				       [ Iteration_expression_Get40
				       | Llist_Get42
				       ],
				       TrueResult84),
	    ElseResult79=TrueResult84
	;   is_eq(Key, u_from)
	->  get_var(LEnv, u_llist, Llist_Get47),
	    cl_fifth(Llist_Get47, IFTEST45),
	    (   IFTEST45==[]
	    ->  set_var(LEnv, u_stepping_variable, 1),
		_61576322=1
	    ;   get_var(LEnv, u_llist, Llist_Get48),
		cl_fifth(Llist_Get48, ElseResult),
		set_var(LEnv, u_stepping_variable, ElseResult),
		_61576322=ElseResult
	    ),
	    get_var(LEnv, u_llist, Llist_Get51),
	    cl_length(Llist_Get51, PredArg1Result53),
	    (   PredArg1Result53>5
	    ->  f_u_lerror('$ARRAY'([*],
				    claz_base_character,
				    "YL:Too many clauses in (yfor ~a ~a ..)"),
			   [variable, u_what_to_do],
			   TrueResult80),
		ElseResult79=TrueResult80
	    ;   get_var(LEnv, u_stepping_variable, Stepping_variable_Get),
		(   mth:is_minusp(Stepping_variable_Get)
		->  get_var(LEnv, u_llist, Llist_Get60),
		    cl_car(Llist_Get60, Car_Ret),
		    get_var(LEnv, u_llist, Llist_Get61),
		    cl_third(Llist_Get61, Third_Ret),
		    <=(Car_Ret, Third_Ret, TrueResult),
		    IFTEST54=TrueResult
		;   IFTEST54=[]
		),
		(   IFTEST54\==[]
		->  f_u_lerror('$ARRAY'([*],
					claz_base_character,
					"YL:Cannot decrement from ~a to ~a"),
			       [[first, u_llist], [third, u_llist]],
			       TrueResult78),
		    ElseResult79=TrueResult78
		;   get_var(LEnv,
			    u_iteration_variable,
			    Iteration_variable_Get63),
		    get_var(LEnv, u_llist, Llist_Get64),
		    cl_car(Llist_Get64, Car_Ret104),
		    f_u_add_element_to_loop_alist(
						  [ Iteration_variable_Get63,
						    Car_Ret104
						  ],
						  u_initializations,
						  Initializations),
		    get_var(LEnv,
			    u_iteration_variable,
			    Iteration_variable_Get65),
		    get_var(LEnv, u_stepping_variable, Stepping_variable_Get67),
		    f_u_add_element_to_loop_alist(
						  [ setf,
						    Iteration_variable_Get65,
						    
						    [ (+),
						      Iteration_variable_Get65,
						      Stepping_variable_Get67
						    ]
						  ],
						  u_next,
						  Next),
		    get_var(LEnv, u_stepping_variable, Stepping_variable_Get69),
		    (   mth:is_minusp(Stepping_variable_Get69)
		    ->  get_var(LEnv,
				u_iteration_variable,
				Iteration_variable_Get72),
			get_var(LEnv, u_llist, Llist_Get73),
			cl_third(Llist_Get73, Third_Ret105),
			f_u_add_element_to_loop_alist(
						      [ (<),
							Iteration_variable_Get72,
							Third_Ret105
						      ],
						      u_end_test,
						      TrueResult76),
			ElseResult79=TrueResult76
		    ;   get_var(LEnv,
				u_iteration_variable,
				Iteration_variable_Get74),
			get_var(LEnv, u_llist, Llist_Get75),
			cl_third(Llist_Get75, Third_Ret106),
			f_u_add_element_to_loop_alist(
						      [ (>),
							Iteration_variable_Get74,
							Third_Ret106
						      ],
						      u_end_test,
						      ElseResult77),
			ElseResult79=ElseResult77
		    )
		)
	    )
	;   ElseResult79=[]
	),
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yfor, classof, claz_macro),
   set_opv(u_yfor, compile_as, kw_operator),
   set_opv(u_yfor, function, f_u_yfor),
   _Ignored4=u_yfor.
/*
:- side_effect(assert_lsp(u_yfor,
			  wl:lambda_def(defmacro, u_yfor, f_u_yfor, [variable, u_what_to_do, c38_rest, u_llist], [progn, [let, [[u_iteration_variable, [gensym]], [u_iteration_expression, [gensym]], u_stepping_variable], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []], [['#COMMA', u_iteration_expression], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [case, [intern, [symbol_name, u_what_to_do]], [u_in, [u_record_in_loop_alist, ['#BQ', [endp, ['#COMMA', u_iteration_expression]]], [quote, u_end_test]], [u_add_elements_to_clause, [quote, u_next], ['#BQ', [setf, ['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [setf, ['#COMMA', u_iteration_expression], [cdr, ['#COMMA', u_iteration_expression]]]]], [u_add_elements_to_clause, [quote, u_initializations], ['#BQ', [['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [['#COMMA', u_iteration_expression], ['#BQ-COMMA-ELIPSE', u_llist]]]]], [u_on, [u_record_in_loop_alist, u_iteration_expression, [quote, u_iteration_control_variable]], [u_record_in_loop_alist, ['#BQ', [endp, ['#COMMA', u_iteration_expression]]], [quote, u_end_test]], [u_add_elements_to_clause, [quote, u_next], ['#BQ', [setf, ['#COMMA', u_iteration_variable], ['#COMMA', u_iteration_expression]]], ['#BQ', [setf, ['#COMMA', u_iteration_expression], [cdr, ['#COMMA', u_iteration_expression]]]]], [u_add_elements_to_clause, [quote, u_initializations], ['#BQ', [['#COMMA', u_iteration_variable], [car, ['#COMMA', u_iteration_expression]]]], ['#BQ', [['#COMMA', u_iteration_expression], ['#BQ-COMMA-ELIPSE', u_llist]]]]], [u_from, [if, [null, [fifth, u_llist]], [setf, u_stepping_variable, 1], [setf, u_stepping_variable, [fifth, u_llist]]], [cond, [[>, [length, u_llist], 5], [u_lerror, '$ARRAY'([*], claz_base_character, "YL:Too many clauses in (yfor ~a ~a ..)"), variable, u_what_to_do]], [[and, [minusp, u_stepping_variable], [<=, [first, u_llist], [third, u_llist]]], [u_lerror, '$ARRAY'([*], claz_base_character, "YL:Cannot decrement from ~a to ~a"), [first, u_llist], [third, u_llist]]], [t, [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', [first, u_llist]]]], [quote, u_initializations]], [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [+, ['#COMMA', u_iteration_variable], ['#COMMA', u_stepping_variable]]]], [quote, u_next]], [if, [minusp, u_stepping_variable], [u_add_element_to_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', [third, u_llist]]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', [third, u_llist]]]], [quote, u_end_test]]]]]]]]]]], t]))).
*/
/*
:- side_effect(assert_lsp(u_yfor,
			  wl:arglist_info(u_yfor, f_u_yfor, [variable, u_what_to_do, c38_rest, u_llist], arginfo{all:[variable, u_what_to_do], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_what_to_do, u_llist], opt:0, req:[variable, u_what_to_do], rest:[u_llist], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_yfor, wl:init_args(2, f_u_yfor))).
*/
/*
(car ,@llist))
*/
/*
 note that since you are in a let*, don't eval the expression twice, use
*/
/*
 the variable that it will be bound to
*/
/*
(defmacro with-incr-or-decr-checking (&body body)
  "Very specialized code to fit in the incr and decr macros."
  `(progn
    (cond
      ((null args)
       (setf final t)
       (setf step 1))
      ((numberp (first args))
       (lerror "Syntax error in incr: expected a yloop keyword after fmt90_x1" init))
      ((not (numberp (second args)))
       (lerror "Syntax error in incr: fmt90_x2 not a number" (second args)))      
      (t (setf final (second args))
         (if (null (fourth args))
             (setf step 1)
             (setf step (fourth args)))))
    ,@body))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:9875 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'with-incr-or-decr-checking',['&body',body],'$STRING'("Very specialized code to fit in the incr and decr macros."),['#BQ',[progn,[cond,[[null,args],[setf,final,t],[setf,step,1]],[[numberp,[first,args]],[lerror,'$STRING'("Syntax error in incr: expected a yloop keyword after ~a"),init]],[[not,[numberp,[second,args]]],[lerror,'$STRING'("Syntax error in incr: ~a not a number"),[second,args]]],[t,[setf,final,[second,args]],[if,[null,[fourth,args]],[setf,step,1],[setf,step,[fourth,args]]]]],['#BQ-COMMA-ELIPSE',body]]]])
doc: doc_string(u_with_incr_or_decr_checking,
	      _68098402,
	      function,
	      "Very specialized code to fit in the incr and decr macros.").

wl:lambda_def(defmacro, u_with_incr_or_decr_checking, f_u_with_incr_or_decr_checking, [c38_body, u_body], [progn, ['#BQ', [progn, [cond, [[null, args], [setf, u_final, t], [setf, step, 1]], [[numberp, [first, args]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: expected a yloop keyword after ~a"), u_init]], [[not, [numberp, [second, args]]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: ~a not a number"), [second, args]]], [t, [setf, u_final, [second, args]], [if, [null, [fourth, args]], [setf, step, 1], [setf, step, [fourth, args]]]]], ['#BQ-COMMA-ELIPSE', u_body]]]]).
wl:arglist_info(u_with_incr_or_decr_checking, f_u_with_incr_or_decr_checking, [c38_body, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:[u_body], complex:[body], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}).
wl: init_args(0, f_u_with_incr_or_decr_checking).

/*

### Compiled:  `U::WITH-INCR-OR-DECR-CHECKING` 
*/
f_u_with_incr_or_decr_checking(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_body, Body)]|Global_env_Ret]|Global_env_Ret],
	as_body(u_body, Body, RestNKeys),
	get_var(GEnv, u_body, Body_Get),
	[progn, [cond, [[null, args], [setf, u_final, t], [setf, step, 1]], [[numberp, [first, args]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: expected a yloop keyword after ~a"), u_init]], [[not, [numberp, [second, args]]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: ~a not a number"), [second, args]]], [t, [setf, u_final, [second, args]], [if, [null, [fourth, args]], [setf, step, 1], [setf, step, [fourth, args]]]]]|Body_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_incr_or_decr_checking, classof, claz_macro),
   set_opv(u_with_incr_or_decr_checking, compile_as, kw_operator),
   set_opv(u_with_incr_or_decr_checking,
	   function,
	   f_u_with_incr_or_decr_checking),
   _Ignored4=u_with_incr_or_decr_checking.
/*
:- side_effect(assert_lsp(u_with_incr_or_decr_checking,
			  doc:doc_string(u_with_incr_or_decr_checking, _68098402, function, "Very specialized code to fit in the incr and decr macros."))).
*/
/*
:- side_effect(assert_lsp(u_with_incr_or_decr_checking,
			  wl:lambda_def(defmacro, u_with_incr_or_decr_checking, f_u_with_incr_or_decr_checking, [c38_body, u_body], [progn, ['#BQ', [progn, [cond, [[null, args], [setf, u_final, t], [setf, step, 1]], [[numberp, [first, args]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: expected a yloop keyword after ~a"), u_init]], [[not, [numberp, [second, args]]], [u_lerror, '$ARRAY'([*], claz_base_character, "Syntax error in incr: ~a not a number"), [second, args]]], [t, [setf, u_final, [second, args]], [if, [null, [fourth, args]], [setf, step, 1], [setf, step, [fourth, args]]]]], ['#BQ-COMMA-ELIPSE', u_body]]]]))).
*/
/*
:- side_effect(assert_lsp(u_with_incr_or_decr_checking,
			  wl:arglist_info(u_with_incr_or_decr_checking, f_u_with_incr_or_decr_checking, [c38_body, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:[u_body], complex:[body], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_with_incr_or_decr_checking,
			  wl:init_args(0, f_u_with_incr_or_decr_checking))).
*/
/*
(defmacro incr (variable from init &rest args) 
  (let* (final step (iteration-variable (gensym)))
    `(let ( (,iteration-variable nil) )
       ,(record-in-loop-alist `(,variable ,iteration-variable)
                              'iteration-variable)
    ,(with-incr-or-decr-checking
      (add-element-to-loop-alist `(setf ,iteration-variable
                                        (+ ,iteration-variable ,step)) 'next)
      (case (intern (symbol-name from))
        (.in. (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,init)
                                         'initializations))
        (.in  (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,init)
                                         'initializations))
        (in.  (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,(1+ init))
                                         'initializations))
        (in   (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
              (add-element-to-loop-alist `(,iteration-variable ,(1+ init))
                                         'initializations))
        (otherwise
          (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
          (add-element-to-loop-alist `(,iteration-variable ,init) 
                                     'initializations))))))
    t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:10447 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,incr,[variable,from,init,'&rest',args],['let*',[final,step,['iteration-variable',[gensym]]],['#BQ',[let,[[['#COMMA','iteration-variable'],[]]],['#COMMA',['record-in-loop-alist',['#BQ',[['#COMMA',variable],['#COMMA','iteration-variable']]],[quote,'iteration-variable']]],['#COMMA',['with-incr-or-decr-checking',['add-element-to-loop-alist',['#BQ',[setf,['#COMMA','iteration-variable'],[+,['#COMMA','iteration-variable'],['#COMMA',step]]]],[quote,next]],[case,[intern,['symbol-name',from]],['.in.',['record-in-loop-alist',['#BQ',[>,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]],['.in',['record-in-loop-alist',['#BQ',[=,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]],['in.',['record-in-loop-alist',['#BQ',[>,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',['1+',init]]]],[quote,initializations]]],[in,['record-in-loop-alist',['#BQ',[=,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',['1+',init]]]],[quote,initializations]]],[otherwise,['record-in-loop-alist',['#BQ',[>,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]]]]]]]],t])
wl:lambda_def(defmacro, u_incr, f_u_incr, [variable, u_from, u_init, c38_rest, args], [progn, [let_xx, [u_final, step, [u_iteration_variable, [gensym]]], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [u_with_incr_or_decr_checking, [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [+, ['#COMMA', u_iteration_variable], ['#COMMA', step]]]], [quote, u_next]], [case, [intern, [symbol_name, u_from]], [u_c46_in_c46, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_c46_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_in_c46, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1+', u_init]]]], [quote, u_initializations]]], [u_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1+', u_init]]]], [quote, u_initializations]]], [otherwise, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]]]]]]]], t]).
wl:arglist_info(u_incr, f_u_incr, [variable, u_from, u_init, c38_rest, args], arginfo{all:[variable, u_from, u_init], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_from, u_init, args], opt:0, req:[variable, u_from, u_init], rest:[args], sublists:0, whole:0}).
wl: init_args(3, f_u_incr).

/*

### Compiled:  `U::INCR` 
*/
f_u_incr(Variable, From, Init, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(variable, Variable), bv(u_from, From), bv(u_init, Init), bv(args, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Iteration_variable_Init),
	LEnv=[bv(u_final, []), bv(step, []), bv(u_iteration_variable, Iteration_variable_Init)|CDR],
	get_var(LEnv, u_iteration_variable, Iteration_variable_Get14),
	get_var(LEnv, variable, Variable_Get),
	f_u_record_in_loop_alist([Variable_Get, Iteration_variable_Get14],
				 u_iteration_variable,
				 Iteration_variable),
	f_u_with_incr_or_decr_checking(
				       [ 
					 [ u_add_element_to_loop_alist,
					   
					   [ '#BQ',
					     
					     [ setf,
					       ['#COMMA', u_iteration_variable],
					       
					       [ (+),
						 
						 [ '#COMMA',
						   u_iteration_variable
						 ],
						 ['#COMMA', step]
					       ]
					     ]
					   ],
					   [quote, u_next]
					 ],
					 
					 [ case,
					   [intern, [symbol_name, u_from]],
					   
					   [ u_c46_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_c46_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1+', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1+', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ otherwise,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ]
					 ]
				       ],
				       Decr_checking_Ret),
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_incr, classof, claz_macro),
   set_opv(u_incr, compile_as, kw_operator),
   set_opv(u_incr, function, f_u_incr),
   _Ignored4=u_incr.
/*
:- side_effect(assert_lsp(u_incr,
			  wl:lambda_def(defmacro, u_incr, f_u_incr, [variable, u_from, u_init, c38_rest, args], [progn, [let_xx, [u_final, step, [u_iteration_variable, [gensym]]], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [u_with_incr_or_decr_checking, [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [+, ['#COMMA', u_iteration_variable], ['#COMMA', step]]]], [quote, u_next]], [case, [intern, [symbol_name, u_from]], [u_c46_in_c46, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_c46_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_in_c46, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1+', u_init]]]], [quote, u_initializations]]], [u_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1+', u_init]]]], [quote, u_initializations]]], [otherwise, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]]]]]]]], t]))).
*/
/*
:- side_effect(assert_lsp(u_incr,
			  wl:arglist_info(u_incr, f_u_incr, [variable, u_from, u_init, c38_rest, args], arginfo{all:[variable, u_from, u_init], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_from, u_init, args], opt:0, req:[variable, u_from, u_init], rest:[args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_incr, wl:init_args(3, f_u_incr))).
*/
/*
(defmacro decr (variable from init &rest args)
  (let (final step (iteration-variable (gensym)))

    `(let ((,iteration-variable nil))
           ,(record-in-loop-alist `(,variable ,iteration-variable) 
                                  'iteration-variable)
    ,(with-incr-or-decr-checking
      (when (<= init final)
        (lerror
       "Cannot decrement from fmt90_x1 downto fmt90_x2. Check the order of your arguments"
       init final))
      (add-element-to-loop-alist
       `(setf ,iteration-variable (- ,iteration-variable ,step))
       'next)
      (case (intern (symbol-name from))
      (.in. (record-in-loop-alist `(< ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations))
      (.in  (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations))
      (in.  (record-in-loop-alist `(< ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,(1- init)) 'initializations))
      (in   (record-in-loop-alist `(= ,iteration-variable ,final) 'end-test)
            (add-element-to-loop-alist `(,iteration-variable ,(1- init)) 'initializations))
      (otherwise
        (record-in-loop-alist `(> ,iteration-variable ,final) 'end-test)
        (add-element-to-loop-alist `(,iteration-variable ,init) 'initializations)))
      ) )) t)
     
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:11986 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,decr,[variable,from,init,'&rest',args],[let,[final,step,['iteration-variable',[gensym]]],['#BQ',[let,[[['#COMMA','iteration-variable'],[]]],['#COMMA',['record-in-loop-alist',['#BQ',[['#COMMA',variable],['#COMMA','iteration-variable']]],[quote,'iteration-variable']]],['#COMMA',['with-incr-or-decr-checking',[when,[<=,init,final],[lerror,'$STRING'("Cannot decrement from ~a downto ~a. Check the order of your arguments"),init,final]],['add-element-to-loop-alist',['#BQ',[setf,['#COMMA','iteration-variable'],[-,['#COMMA','iteration-variable'],['#COMMA',step]]]],[quote,next]],[case,[intern,['symbol-name',from]],['.in.',['record-in-loop-alist',['#BQ',[<,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]],['.in',['record-in-loop-alist',['#BQ',[=,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]],['in.',['record-in-loop-alist',['#BQ',[<,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',['1-',init]]]],[quote,initializations]]],[in,['record-in-loop-alist',['#BQ',[=,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',['1-',init]]]],[quote,initializations]]],[otherwise,['record-in-loop-alist',['#BQ',[>,['#COMMA','iteration-variable'],['#COMMA',final]]],[quote,'end-test']],['add-element-to-loop-alist',['#BQ',[['#COMMA','iteration-variable'],['#COMMA',init]]],[quote,initializations]]]]]]]]],t])
wl:lambda_def(defmacro, u_decr, f_u_decr, [variable, u_from, u_init, c38_rest, args], [progn, [let, [u_final, step, [u_iteration_variable, [gensym]]], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [u_with_incr_or_decr_checking, [when, [<=, u_init, u_final], [u_lerror, '$ARRAY'([*], claz_base_character, "Cannot decrement from ~a downto ~a. Check the order of your arguments"), u_init, u_final]], [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [-, ['#COMMA', u_iteration_variable], ['#COMMA', step]]]], [quote, u_next]], [case, [intern, [symbol_name, u_from]], [u_c46_in_c46, [u_record_in_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_c46_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_in_c46, [u_record_in_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1-', u_init]]]], [quote, u_initializations]]], [u_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1-', u_init]]]], [quote, u_initializations]]], [otherwise, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]]]]]]]], t]).
wl:arglist_info(u_decr, f_u_decr, [variable, u_from, u_init, c38_rest, args], arginfo{all:[variable, u_from, u_init], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_from, u_init, args], opt:0, req:[variable, u_from, u_init], rest:[args], sublists:0, whole:0}).
wl: init_args(3, f_u_decr).

/*

### Compiled:  `U::DECR` 
*/
f_u_decr(Variable, From, Init, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(variable, Variable), bv(u_from, From), bv(u_init, Init), bv(args, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Iteration_variable_Init),
	LEnv=[bv(u_final, []), bv(step, []), bv(u_iteration_variable, Iteration_variable_Init)|CDR],
	get_var(LEnv, u_iteration_variable, Iteration_variable_Get14),
	get_var(LEnv, variable, Variable_Get),
	f_u_record_in_loop_alist([Variable_Get, Iteration_variable_Get14],
				 u_iteration_variable,
				 Iteration_variable),
	f_u_with_incr_or_decr_checking(
				       [ 
					 [ when,
					   [<=, u_init, u_final],
					   
					   [ u_lerror,
					     '$ARRAY'([*],
						      claz_base_character,
						      "Cannot decrement from ~a downto ~a. Check the order of your arguments"),
					     u_init,
					     u_final
					   ]
					 ],
					 
					 [ u_add_element_to_loop_alist,
					   
					   [ '#BQ',
					     
					     [ setf,
					       ['#COMMA', u_iteration_variable],
					       
					       [ (-),
						 
						 [ '#COMMA',
						   u_iteration_variable
						 ],
						 ['#COMMA', step]
					       ]
					     ]
					   ],
					   [quote, u_next]
					 ],
					 
					 [ case,
					   [intern, [symbol_name, u_from]],
					   
					   [ u_c46_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (<),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_c46_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in_c46,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (<),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1-', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ u_in,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (=),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', ['1-', u_init]]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ],
					   
					   [ otherwise,
					     
					     [ u_record_in_loop_alist,
					       
					       [ '#BQ',
						 
						 [ (>),
						   
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_final]
						 ]
					       ],
					       [quote, u_end_test]
					     ],
					     
					     [ u_add_element_to_loop_alist,
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#COMMA',
						     u_iteration_variable
						   ],
						   ['#COMMA', u_init]
						 ]
					       ],
					       [quote, u_initializations]
					     ]
					   ]
					 ]
				       ],
				       Decr_checking_Ret),
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_decr, classof, claz_macro),
   set_opv(u_decr, compile_as, kw_operator),
   set_opv(u_decr, function, f_u_decr),
   _Ignored4=u_decr.
/*
:- side_effect(assert_lsp(u_decr,
			  wl:lambda_def(defmacro, u_decr, f_u_decr, [variable, u_from, u_init, c38_rest, args], [progn, [let, [u_final, step, [u_iteration_variable, [gensym]]], ['#BQ', [let, [[['#COMMA', u_iteration_variable], []]], ['#COMMA', [u_record_in_loop_alist, ['#BQ', [['#COMMA', variable], ['#COMMA', u_iteration_variable]]], [quote, u_iteration_variable]]], ['#COMMA', [u_with_incr_or_decr_checking, [when, [<=, u_init, u_final], [u_lerror, '$ARRAY'([*], claz_base_character, "Cannot decrement from ~a downto ~a. Check the order of your arguments"), u_init, u_final]], [u_add_element_to_loop_alist, ['#BQ', [setf, ['#COMMA', u_iteration_variable], [-, ['#COMMA', u_iteration_variable], ['#COMMA', step]]]], [quote, u_next]], [case, [intern, [symbol_name, u_from]], [u_c46_in_c46, [u_record_in_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_c46_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]], [u_in_c46, [u_record_in_loop_alist, ['#BQ', [<, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1-', u_init]]]], [quote, u_initializations]]], [u_in, [u_record_in_loop_alist, ['#BQ', [=, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', ['1-', u_init]]]], [quote, u_initializations]]], [otherwise, [u_record_in_loop_alist, ['#BQ', [>, ['#COMMA', u_iteration_variable], ['#COMMA', u_final]]], [quote, u_end_test]], [u_add_element_to_loop_alist, ['#BQ', [['#COMMA', u_iteration_variable], ['#COMMA', u_init]]], [quote, u_initializations]]]]]]]]], t]))).
*/
/*
:- side_effect(assert_lsp(u_decr,
			  wl:arglist_info(u_decr, f_u_decr, [variable, u_from, u_init, c38_rest, args], arginfo{all:[variable, u_from, u_init], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[variable, u_from, u_init, args], opt:0, req:[variable, u_from, u_init], rest:[args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_decr, wl:init_args(3, f_u_decr))).
*/
/*
(defmacro after (&rest clauses)
  `(add-element-to-loop-alist (cons 'progn ',clauses) 'after))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13433 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,after,['&rest',clauses],['#BQ',['add-element-to-loop-alist',[cons,[quote,progn],[quote,['#COMMA',clauses]]],[quote,after]]]])
wl:lambda_def(defmacro, u_after, f_u_after, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, u_after]]]]).
wl:arglist_info(u_after, f_u_after, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}).
wl: init_args(0, f_u_after).

/*

### Compiled:  `U::AFTER` 
*/
f_u_after(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_clauses, Clauses_Get),
	[u_add_element_to_loop_alist, [cons, [quote, progn], [quote, Clauses_Get]], [quote, u_after]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_after, classof, claz_macro),
   set_opv(u_after, compile_as, kw_operator),
   set_opv(u_after, function, f_u_after),
   _Ignored4=u_after.
/*
:- side_effect(assert_lsp(u_after,
			  wl:lambda_def(defmacro, u_after, f_u_after, [c38_rest, u_clauses], [progn, ['#BQ', [u_add_element_to_loop_alist, [cons, [quote, progn], [quote, ['#COMMA', u_clauses]]], [quote, u_after]]]]))).
*/
/*
:- side_effect(assert_lsp(u_after,
			  wl:arglist_info(u_after, f_u_after, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_after, wl:init_args(0, f_u_after))).
*/
/*
(defun fetch-new-iteration-variable ()
  (second (car (fetch-clauses iteration-variable))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13529 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'fetch-new-iteration-variable',[],[second,[car,['fetch-clauses','iteration-variable']]]])
wl:lambda_def(defun, u_fetch_new_iteration_variable, f_u_fetch_new_iteration_variable, [], [[second, [car, [u_fetch_clauses, u_iteration_variable]]]]).
wl:arglist_info(u_fetch_new_iteration_variable, f_u_fetch_new_iteration_variable, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fetch_new_iteration_variable).

/*

### Compiled:  `U::FETCH-NEW-ITERATION-VARIABLE` 
*/
f_u_fetch_new_iteration_variable(FnResult) :-
	nop(global_env(Env)),
	_76283284=Env,
	f_u_fetch_clauses(u_iteration_variable, Car_Param),
	cl_car(Car_Param, Second_Param),
	cl_second(Second_Param, Second_Ret),
	Second_Ret=FnResult.
:- set_opv(f_u_fetch_new_iteration_variable, classof, claz_function),
   set_opv(u_fetch_new_iteration_variable, compile_as, kw_function),
   set_opv(u_fetch_new_iteration_variable,
	   function,
	   f_u_fetch_new_iteration_variable),
   _Ignored4=u_fetch_new_iteration_variable.
/*
:- side_effect(assert_lsp(u_fetch_new_iteration_variable,
			  wl:lambda_def(defun, u_fetch_new_iteration_variable, f_u_fetch_new_iteration_variable, [], [[second, [car, [u_fetch_clauses, u_iteration_variable]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fetch_new_iteration_variable,
			  wl:arglist_info(u_fetch_new_iteration_variable, f_u_fetch_new_iteration_variable, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fetch_new_iteration_variable,
			  wl:init_args(exact_only, f_u_fetch_new_iteration_variable))).
*/
/*
(defun fetch-old-iteration-variable ()
  (first (car (fetch-clauses iteration-variable))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13622 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'fetch-old-iteration-variable',[],[first,[car,['fetch-clauses','iteration-variable']]]])
wl:lambda_def(defun, u_fetch_old_iteration_variable, f_u_fetch_old_iteration_variable, [], [[first, [car, [u_fetch_clauses, u_iteration_variable]]]]).
wl:arglist_info(u_fetch_old_iteration_variable, f_u_fetch_old_iteration_variable, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fetch_old_iteration_variable).

/*

### Compiled:  `U::FETCH-OLD-ITERATION-VARIABLE` 
*/
f_u_fetch_old_iteration_variable(FnResult) :-
	nop(global_env(Env)),
	_76756820=Env,
	f_u_fetch_clauses(u_iteration_variable, Car_Param),
	cl_car(Car_Param, Car_Param10),
	cl_car(Car_Param10, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_fetch_old_iteration_variable, classof, claz_function),
   set_opv(u_fetch_old_iteration_variable, compile_as, kw_function),
   set_opv(u_fetch_old_iteration_variable,
	   function,
	   f_u_fetch_old_iteration_variable),
   _Ignored4=u_fetch_old_iteration_variable.
/*
:- side_effect(assert_lsp(u_fetch_old_iteration_variable,
			  wl:lambda_def(defun, u_fetch_old_iteration_variable, f_u_fetch_old_iteration_variable, [], [[first, [car, [u_fetch_clauses, u_iteration_variable]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fetch_old_iteration_variable,
			  wl:arglist_info(u_fetch_old_iteration_variable, f_u_fetch_old_iteration_variable, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fetch_old_iteration_variable,
			  wl:init_args(exact_only, f_u_fetch_old_iteration_variable))).
*/
/*
(defun record-in-loop-alist (element key)
  "Adds new assoc pairs in *loop-alist*."
  (acons-setf (list element) key *loop-alist*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13714 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'record-in-loop-alist',[element,key],'$STRING'("Adds new assoc pairs in *loop-alist*."),['acons-setf',[list,element],key,'*loop-alist*']])
doc: doc_string(u_record_in_loop_alist,
	      _77236726,
	      function,
	      "Adds new assoc pairs in *loop-alist*.").

wl:lambda_def(defun, u_record_in_loop_alist, f_u_record_in_loop_alist, [u_element, key], [[u_acons_setf, [list, u_element], key, u_xx_loop_alist_xx]]).
wl:arglist_info(u_record_in_loop_alist, f_u_record_in_loop_alist, [u_element, key], arginfo{all:[u_element, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, key], opt:0, req:[u_element, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_record_in_loop_alist).

/*

### Compiled:  `U::RECORD-IN-LOOP-ALIST` 
*/
f_u_record_in_loop_alist(Element, Key, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_element, Element), bv(key, Key)|Env],
	f_u_acons_setf([list, u_element],
		       key,
		       u_xx_loop_alist_xx,
		       Xx_loop_alist_xx),
	Xx_loop_alist_xx=FnResult.
:- set_opv(f_u_record_in_loop_alist, classof, claz_function),
   set_opv(u_record_in_loop_alist, compile_as, kw_function),
   set_opv(u_record_in_loop_alist, function, f_u_record_in_loop_alist),
   _Ignored4=u_record_in_loop_alist.
/*
:- side_effect(assert_lsp(u_record_in_loop_alist,
			  doc:doc_string(u_record_in_loop_alist, _77236726, function, "Adds new assoc pairs in *loop-alist*."))).
*/
/*
:- side_effect(assert_lsp(u_record_in_loop_alist,
			  wl:lambda_def(defun, u_record_in_loop_alist, f_u_record_in_loop_alist, [u_element, key], [[u_acons_setf, [list, u_element], key, u_xx_loop_alist_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_record_in_loop_alist,
			  wl:arglist_info(u_record_in_loop_alist, f_u_record_in_loop_alist, [u_element, key], arginfo{all:[u_element, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, key], opt:0, req:[u_element, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_record_in_loop_alist,
			  wl:init_args(exact_only, f_u_record_in_loop_alist))).
*/
/*
(defun add-element-to-loop-alist (element clause-key)
  "Adds elements to a particular assoc sublist."
  (cond
    ((null (rassoc clause-key *loop-alist*))
     (record-in-loop-alist element clause-key))
    (t (rplaca (rassoc clause-key *loop-alist*)
               (cons element (car (rassoc clause-key *loop-alist*)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:13847 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-element-to-loop-alist',[element,'clause-key'],'$STRING'("Adds elements to a particular assoc sublist."),[cond,[[null,[rassoc,'clause-key','*loop-alist*']],['record-in-loop-alist',element,'clause-key']],[t,[rplaca,[rassoc,'clause-key','*loop-alist*'],[cons,element,[car,[rassoc,'clause-key','*loop-alist*']]]]]]])
doc: doc_string(u_add_element_to_loop_alist,
	      _77831750,
	      function,
	      "Adds elements to a particular assoc sublist.").

wl:lambda_def(defun, u_add_element_to_loop_alist, f_u_add_element_to_loop_alist, [u_element, u_clause_key], [[cond, [[null, [rassoc, u_clause_key, u_xx_loop_alist_xx]], [u_record_in_loop_alist, u_element, u_clause_key]], [t, [rplaca, [rassoc, u_clause_key, u_xx_loop_alist_xx], [cons, u_element, [car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]]]]]]).
wl:arglist_info(u_add_element_to_loop_alist, f_u_add_element_to_loop_alist, [u_element, u_clause_key], arginfo{all:[u_element, u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, u_clause_key], opt:0, req:[u_element, u_clause_key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_add_element_to_loop_alist).

/*

### Compiled:  `U::ADD-ELEMENT-TO-LOOP-ALIST` 
*/
f_u_add_element_to_loop_alist(Element, Clause_key, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_element, Element), bv(u_clause_key, Clause_key)|Env],
	get_var(Env22, u_clause_key, Clause_key_Get),
	get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(Clause_key_Get, Xx_loop_alist_xx_Get, [], IFTEST),
	(   IFTEST==[]
	->  get_var(Env22, u_clause_key, Clause_key_Get12),
	    get_var(Env22, u_element, Element_Get),
	    f_u_record_in_loop_alist(Element_Get, Clause_key_Get12, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env22, u_clause_key, Clause_key_Get13),
	    get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get14),
	    cl_rassoc(Clause_key_Get13, Xx_loop_alist_xx_Get14, [], Rplaca_Param),
	    get_var(Env22, u_clause_key, Clause_key_Get16),
	    get_var(Env22, u_element, Element_Get15),
	    get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get17),
	    cl_rassoc(Clause_key_Get16, Xx_loop_alist_xx_Get17, [], Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    _77862384=[Element_Get15|Car_Ret],
	    cl_rplaca(Rplaca_Param, _77862384, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_element_to_loop_alist, classof, claz_function),
   set_opv(u_add_element_to_loop_alist, compile_as, kw_function),
   set_opv(u_add_element_to_loop_alist, function, f_u_add_element_to_loop_alist),
   _Ignored4=u_add_element_to_loop_alist.
/*
:- side_effect(assert_lsp(u_add_element_to_loop_alist,
			  doc:doc_string(u_add_element_to_loop_alist, _77831750, function, "Adds elements to a particular assoc sublist."))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_loop_alist,
			  wl:lambda_def(defun, u_add_element_to_loop_alist, f_u_add_element_to_loop_alist, [u_element, u_clause_key], [[cond, [[null, [rassoc, u_clause_key, u_xx_loop_alist_xx]], [u_record_in_loop_alist, u_element, u_clause_key]], [t, [rplaca, [rassoc, u_clause_key, u_xx_loop_alist_xx], [cons, u_element, [car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_loop_alist,
			  wl:arglist_info(u_add_element_to_loop_alist, f_u_add_element_to_loop_alist, [u_element, u_clause_key], arginfo{all:[u_element, u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, u_clause_key], opt:0, req:[u_element, u_clause_key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_loop_alist,
			  wl:init_args(exact_only, f_u_add_element_to_loop_alist))).
*/
/*
(defun add-elements-to-end-of-clause (clause-key &rest elements)
  (dolist (element elements) (add-element-to-end-of-loop-alist element clause-key)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14173 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-elements-to-end-of-clause',['clause-key','&rest',elements],[dolist,[element,elements],['add-element-to-end-of-loop-alist',element,'clause-key']]])
wl:lambda_def(defun, u_add_elements_to_end_of_clause, f_u_add_elements_to_end_of_clause, [u_clause_key, c38_rest, u_elements], [[dolist, [u_element, u_elements], [u_add_element_to_end_of_loop_alist, u_element, u_clause_key]]]).
wl:arglist_info(u_add_elements_to_end_of_clause, f_u_add_elements_to_end_of_clause, [u_clause_key, c38_rest, u_elements], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clause_key, u_elements], opt:0, req:[u_clause_key], rest:[u_elements], sublists:0, whole:0}).
wl: init_args(1, f_u_add_elements_to_end_of_clause).

/*

### Compiled:  `U::ADD-ELEMENTS-TO-END-OF-CLAUSE` 
*/
f_u_add_elements_to_end_of_clause(Clause_key, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(u_clause_key, Clause_key), bv(u_elements, RestNKeys)]|Env]|Env],
	get_var(GEnv, u_elements, Elements_Get),
	BV=bv(u_element, Ele),
	Env2=[BV|GEnv],
	forall(member(Ele, Elements_Get),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env2, u_clause_key, Clause_key_Get),
		 get_var(Env2, u_element, Element_Get),
		 f_u_add_element_to_end_of_loop_alist(Element_Get,
						      Clause_key_Get,
						      Loop_alist_Ret)
	       )),
	Loop_alist_Ret=FnResult.
:- set_opv(f_u_add_elements_to_end_of_clause, classof, claz_function),
   set_opv(u_add_elements_to_end_of_clause, compile_as, kw_function),
   set_opv(u_add_elements_to_end_of_clause,
	   function,
	   f_u_add_elements_to_end_of_clause),
   _Ignored4=u_add_elements_to_end_of_clause.
/*
:- side_effect(assert_lsp(u_add_elements_to_end_of_clause,
			  wl:lambda_def(defun, u_add_elements_to_end_of_clause, f_u_add_elements_to_end_of_clause, [u_clause_key, c38_rest, u_elements], [[dolist, [u_element, u_elements], [u_add_element_to_end_of_loop_alist, u_element, u_clause_key]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_elements_to_end_of_clause,
			  wl:arglist_info(u_add_elements_to_end_of_clause, f_u_add_elements_to_end_of_clause, [u_clause_key, c38_rest, u_elements], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clause_key, u_elements], opt:0, req:[u_clause_key], rest:[u_elements], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_elements_to_end_of_clause,
			  wl:init_args(1, f_u_add_elements_to_end_of_clause))).
*/
/*
(defun add-elements-to-clause (clause-key &rest elements)
  (dolist (element elements) (add-element-to-loop-alist element clause-key)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14324 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-elements-to-clause',['clause-key','&rest',elements],[dolist,[element,elements],['add-element-to-loop-alist',element,'clause-key']]])
wl:lambda_def(defun, u_add_elements_to_clause, f_u_add_elements_to_clause, [u_clause_key, c38_rest, u_elements], [[dolist, [u_element, u_elements], [u_add_element_to_loop_alist, u_element, u_clause_key]]]).
wl:arglist_info(u_add_elements_to_clause, f_u_add_elements_to_clause, [u_clause_key, c38_rest, u_elements], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clause_key, u_elements], opt:0, req:[u_clause_key], rest:[u_elements], sublists:0, whole:0}).
wl: init_args(1, f_u_add_elements_to_clause).

/*

### Compiled:  `U::ADD-ELEMENTS-TO-CLAUSE` 
*/
f_u_add_elements_to_clause(Clause_key, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(u_clause_key, Clause_key), bv(u_elements, RestNKeys)]|Env]|Env],
	get_var(GEnv, u_elements, Elements_Get),
	BV=bv(u_element, Ele),
	Env2=[BV|GEnv],
	forall(member(Ele, Elements_Get),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env2, u_clause_key, Clause_key_Get),
		 get_var(Env2, u_element, Element_Get),
		 f_u_add_element_to_loop_alist(Element_Get,
					       Clause_key_Get,
					       Loop_alist_Ret)
	       )),
	Loop_alist_Ret=FnResult.
:- set_opv(f_u_add_elements_to_clause, classof, claz_function),
   set_opv(u_add_elements_to_clause, compile_as, kw_function),
   set_opv(u_add_elements_to_clause, function, f_u_add_elements_to_clause),
   _Ignored4=u_add_elements_to_clause.
/*
:- side_effect(assert_lsp(u_add_elements_to_clause,
			  wl:lambda_def(defun, u_add_elements_to_clause, f_u_add_elements_to_clause, [u_clause_key, c38_rest, u_elements], [[dolist, [u_element, u_elements], [u_add_element_to_loop_alist, u_element, u_clause_key]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_elements_to_clause,
			  wl:arglist_info(u_add_elements_to_clause, f_u_add_elements_to_clause, [u_clause_key, c38_rest, u_elements], arginfo{all:[u_clause_key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clause_key, u_elements], opt:0, req:[u_clause_key], rest:[u_elements], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_elements_to_clause,
			  wl:init_args(1, f_u_add_elements_to_clause))).
*/
/*
(defun substitute-iteration-variable (list)
  "Substitutes iteration variables with those given by gensym."
  (let* (
         
        ;; Worring about the effect of (subst..) on ((#) . iteration-variable) 
        ;; the hard way  (sublis ..) may work better
         
        (saved-iteration-variable-clause (rassoc 'iteration-variable
                                                 *loop-alist*))
        (new-iteration-variable-symbol (fetch-new-iteration-variable))
        (old-iteration-variable-symbol (fetch-old-iteration-variable))
        (secured-list (remove (rassoc 'iteration-variable *loop-alist*)
                              list))
        )
    (cond
      ((null (or  new-iteration-variable-symbol old-iteration-variable-symbol))
       (lerror "No iteration variable defined"))  ;;; should not be required -fr
      (t (cons saved-iteration-variable-clause
               (subst new-iteration-variable-symbol
                      old-iteration-variable-symbol secured-list)))))) 

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:14461 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'substitute-iteration-variable',[list],'$STRING'("Substitutes iteration variables with those given by gensym."),['let*',[['saved-iteration-variable-clause',[rassoc,[quote,'iteration-variable'],'*loop-alist*']],['new-iteration-variable-symbol',['fetch-new-iteration-variable']],['old-iteration-variable-symbol',['fetch-old-iteration-variable']],['secured-list',[remove,[rassoc,[quote,'iteration-variable'],'*loop-alist*'],list]]],[cond,[[null,[or,'new-iteration-variable-symbol','old-iteration-variable-symbol']],[lerror,'$STRING'("No iteration variable defined")]],[t,[cons,'saved-iteration-variable-clause',[subst,'new-iteration-variable-symbol','old-iteration-variable-symbol','secured-list']]]]]])
doc: doc_string(u_substitute_iteration_variable,
	      _80543564,
	      function,
	      "Substitutes iteration variables with those given by gensym.").

wl:lambda_def(defun, u_substitute_iteration_variable, f_u_substitute_iteration_variable, [list], [[let_xx, [[u_saved_iteration_variable_clause, [rassoc, [quote, u_iteration_variable], u_xx_loop_alist_xx]], [u_new_iteration_variable_symbol, [u_fetch_new_iteration_variable]], [u_old_iteration_variable_symbol, [u_fetch_old_iteration_variable]], [u_secured_list, [remove, [rassoc, [quote, u_iteration_variable], u_xx_loop_alist_xx], list]]], [cond, [[null, [or, u_new_iteration_variable_symbol, u_old_iteration_variable_symbol]], [u_lerror, '$ARRAY'([*], claz_base_character, "No iteration variable defined")]], [t, [cons, u_saved_iteration_variable_clause, [subst, u_new_iteration_variable_symbol, u_old_iteration_variable_symbol, u_secured_list]]]]]]).
wl:arglist_info(u_substitute_iteration_variable, f_u_substitute_iteration_variable, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_substitute_iteration_variable).

/*

### Compiled:  `U::SUBSTITUTE-ITERATION-VARIABLE` 
*/
f_u_substitute_iteration_variable(List, FnResult) :-
	nop(global_env(Env)),
	Env30=[bv(list, List)|Env],
	get_var(Env30, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(u_iteration_variable,
		  Xx_loop_alist_xx_Get,
		  [],
		  Saved_iteration_variable_clause_Init),
	f_u_fetch_new_iteration_variable(New_iteration_variable_symbol_Init),
	f_u_fetch_old_iteration_variable(Old_iteration_variable_symbol_Init),
	get_var(Env30, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get11),
	cl_rassoc(u_iteration_variable, Xx_loop_alist_xx_Get11, [], Remove_Param),
	get_var(Env30, list, List_Get),
	cl_remove(Remove_Param, List_Get, Secured_list_Init),
	LEnv=[bv(u_saved_iteration_variable_clause, Saved_iteration_variable_clause_Init), bv(u_new_iteration_variable_symbol, New_iteration_variable_symbol_Init), bv(u_old_iteration_variable_symbol, Old_iteration_variable_symbol_Init), bv(u_secured_list, Secured_list_Init)|Env30],
	(   get_var(LEnv,
		    u_new_iteration_variable_symbol,
		    New_iteration_variable_symbol_Get),
	    New_iteration_variable_symbol_Get\==[],
	    IFTEST=New_iteration_variable_symbol_Get
	->  true
	;   get_var(LEnv,
		    u_old_iteration_variable_symbol,
		    Old_iteration_variable_symbol_Get),
	    IFTEST=Old_iteration_variable_symbol_Get
	),
	(   IFTEST==[]
	->  f_u_lerror('$ARRAY'([*],
				claz_base_character,
				"No iteration variable defined"),
		       [],
		       TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv,
		    u_new_iteration_variable_symbol,
		    New_iteration_variable_symbol_Get23),
	    get_var(LEnv,
		    u_old_iteration_variable_symbol,
		    Old_iteration_variable_symbol_Get24),
	    get_var(LEnv,
		    u_saved_iteration_variable_clause,
		    Saved_iteration_variable_clause_Get),
	    get_var(LEnv, u_secured_list, Secured_list_Get),
	    cl_subst(New_iteration_variable_symbol_Get23,
		     Old_iteration_variable_symbol_Get24,
		     Secured_list_Get,
		     Subst_Ret),
	    FnResult=[Saved_iteration_variable_clause_Get|Subst_Ret]
	).
:- set_opv(f_u_substitute_iteration_variable, classof, claz_function),
   set_opv(u_substitute_iteration_variable, compile_as, kw_function),
   set_opv(u_substitute_iteration_variable,
	   function,
	   f_u_substitute_iteration_variable),
   _Ignored4=u_substitute_iteration_variable.
/*
:- side_effect(assert_lsp(u_substitute_iteration_variable,
			  doc:doc_string(u_substitute_iteration_variable, _80543564, function, "Substitutes iteration variables with those given by gensym."))).
*/
/*
:- side_effect(assert_lsp(u_substitute_iteration_variable,
			  wl:lambda_def(defun, u_substitute_iteration_variable, f_u_substitute_iteration_variable, [list], [[let_xx, [[u_saved_iteration_variable_clause, [rassoc, [quote, u_iteration_variable], u_xx_loop_alist_xx]], [u_new_iteration_variable_symbol, [u_fetch_new_iteration_variable]], [u_old_iteration_variable_symbol, [u_fetch_old_iteration_variable]], [u_secured_list, [remove, [rassoc, [quote, u_iteration_variable], u_xx_loop_alist_xx], list]]], [cond, [[null, [or, u_new_iteration_variable_symbol, u_old_iteration_variable_symbol]], [u_lerror, '$ARRAY'([*], claz_base_character, "No iteration variable defined")]], [t, [cons, u_saved_iteration_variable_clause, [subst, u_new_iteration_variable_symbol, u_old_iteration_variable_symbol, u_secured_list]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_substitute_iteration_variable,
			  wl:arglist_info(u_substitute_iteration_variable, f_u_substitute_iteration_variable, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_substitute_iteration_variable,
			  wl:init_args(exact_only, f_u_substitute_iteration_variable))).
*/
/*
; Worring about the effect of (subst..) on ((#) . iteration-variable) 
*/
/*
; the hard way  (sublis ..) may work better
*/
/*
;; should not be required -fr
*/
/*
(defun iteration-variable-exists-p ()
  (fetch-clauses iteration-variable))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15469 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'iteration-variable-exists-p',[],['fetch-clauses','iteration-variable']])
wl:lambda_def(defun, u_iteration_variable_exists_p, f_u_iteration_variable_exists_p, [], [[u_fetch_clauses, u_iteration_variable]]).
wl:arglist_info(u_iteration_variable_exists_p, f_u_iteration_variable_exists_p, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_iteration_variable_exists_p).

/*

### Compiled:  `U::ITERATION-VARIABLE-EXISTS-P` 
*/
f_u_iteration_variable_exists_p(FnResult) :-
	nop(global_env(Env)),
	_82440460=Env,
	f_u_fetch_clauses(u_iteration_variable, Fetch_clauses_Ret),
	Fetch_clauses_Ret=FnResult.
:- set_opv(f_u_iteration_variable_exists_p, classof, claz_function),
   set_opv(u_iteration_variable_exists_p, compile_as, kw_function),
   set_opv(u_iteration_variable_exists_p,
	   function,
	   f_u_iteration_variable_exists_p),
   _Ignored4=u_iteration_variable_exists_p.
/*
:- side_effect(assert_lsp(u_iteration_variable_exists_p,
			  wl:lambda_def(defun, u_iteration_variable_exists_p, f_u_iteration_variable_exists_p, [], [[u_fetch_clauses, u_iteration_variable]]))).
*/
/*
:- side_effect(assert_lsp(u_iteration_variable_exists_p,
			  wl:arglist_info(u_iteration_variable_exists_p, f_u_iteration_variable_exists_p, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_iteration_variable_exists_p,
			  wl:init_args(exact_only, f_u_iteration_variable_exists_p))).
*/
/*
(defmacro yloop (&rest clauses)
  (setf *loop-alist* nil)
  (mapcar 'eval clauses)
  (when (iteration-variable-exists-p)
    ;; you have an iteration variuable to subsitute
    (setf *loop-alist* (substitute-iteration-variable *loop-alist*)))
  (let
    (
     (dos (fetch-clauses do))
     (afters (fetch-clauses after))
     (end-tests (fetch-clauses end-test))
     (bindings (fetch-clauses initializations))
     (result (fetch-clauses result))
     (nexts (fetch-clauses next))
     (befores (fetch-clauses before))
     (middle-stuff (fetch-clauses middle))
     (front-stuff (fetch-clauses front))
     (end-stuff (fetch-clauses end))
     (block-label (gensym))
    )
    (setf *loop-alist* nil)
    ;; if there are multiple end-test's, accomidate them
    ;; with an or wrapped around the end-test    
    `(unwind-protect
         (block ,block-label
           (let* (,@bindings)
             ,@befores ,@front-stuff
             ;if you have nothing to do, jump -fr
             (if (or ,@end-tests)
                 (return-from ,block-label ,@(or result '(nil))))
             (tagbody loop
                      ,@dos ,@middle-stuff ,@nexts
                      (if (or ,@end-tests) nil (go loop)))
             ,@afters ,@end-stuff
             ;; return results or nil              
             (return-from ,block-label ,@(or result '(nil)))))
       ,(clear-loop-alist))
      ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:15546 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,yloop,['&rest',clauses],[setf,'*loop-alist*',[]],[mapcar,[quote,eval],clauses],[when,['iteration-variable-exists-p'],[setf,'*loop-alist*',['substitute-iteration-variable','*loop-alist*']]],[let,[[dos,['fetch-clauses',do]],[afters,['fetch-clauses',after]],['end-tests',['fetch-clauses','end-test']],[bindings,['fetch-clauses',initializations]],[result,['fetch-clauses',result]],[nexts,['fetch-clauses',next]],[befores,['fetch-clauses',before]],['middle-stuff',['fetch-clauses',middle]],['front-stuff',['fetch-clauses',front]],['end-stuff',['fetch-clauses',end]],['block-label',[gensym]]],[setf,'*loop-alist*',[]],['#BQ',['unwind-protect',[block,['#COMMA','block-label'],['let*',[['#BQ-COMMA-ELIPSE',bindings]],['#BQ-COMMA-ELIPSE',befores],['#BQ-COMMA-ELIPSE','front-stuff'],[if,[or,['#BQ-COMMA-ELIPSE','end-tests']],['return-from',['#COMMA','block-label'],['#BQ-COMMA-ELIPSE',[or,result,[quote,[[]]]]]]],[tagbody,loop,['#BQ-COMMA-ELIPSE',dos],['#BQ-COMMA-ELIPSE','middle-stuff'],['#BQ-COMMA-ELIPSE',nexts],[if,[or,['#BQ-COMMA-ELIPSE','end-tests']],[],[go,loop]]],['#BQ-COMMA-ELIPSE',afters],['#BQ-COMMA-ELIPSE','end-stuff'],['return-from',['#COMMA','block-label'],['#BQ-COMMA-ELIPSE',[or,result,[quote,[[]]]]]]]],['#COMMA',['clear-loop-alist']]]]]])
wl:lambda_def(defmacro, u_yloop, f_u_yloop, [c38_rest, u_clauses], [progn, [setf, u_xx_loop_alist_xx, []], [mapcar, [quote, eval], u_clauses], [when, [u_iteration_variable_exists_p], [setf, u_xx_loop_alist_xx, [u_substitute_iteration_variable, u_xx_loop_alist_xx]]], [let, [[u_dos, [u_fetch_clauses, do]], [u_afters, [u_fetch_clauses, u_after]], [u_end_tests, [u_fetch_clauses, u_end_test]], [bindings, [u_fetch_clauses, u_initializations]], [u_result, [u_fetch_clauses, u_result]], [u_nexts, [u_fetch_clauses, u_next]], [u_befores, [u_fetch_clauses, u_before]], [u_middle_stuff, [u_fetch_clauses, u_middle]], [u_front_stuff, [u_fetch_clauses, u_front]], [u_end_stuff, [u_fetch_clauses, end]], [u_block_label, [gensym]]], [setf, u_xx_loop_alist_xx, []], ['#BQ', [unwind_protect, [block, ['#COMMA', u_block_label], [let_xx, [['#BQ-COMMA-ELIPSE', bindings]], ['#BQ-COMMA-ELIPSE', u_befores], ['#BQ-COMMA-ELIPSE', u_front_stuff], [if, [or, ['#BQ-COMMA-ELIPSE', u_end_tests]], [return_from, ['#COMMA', u_block_label], ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]]], [tagbody, loop, ['#BQ-COMMA-ELIPSE', u_dos], ['#BQ-COMMA-ELIPSE', u_middle_stuff], ['#BQ-COMMA-ELIPSE', u_nexts], [if, [or, ['#BQ-COMMA-ELIPSE', u_end_tests]], [], [go, loop]]], ['#BQ-COMMA-ELIPSE', u_afters], ['#BQ-COMMA-ELIPSE', u_end_stuff], [return_from, ['#COMMA', u_block_label], ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]]]], ['#COMMA', [u_clear_loop_alist]]]]]]).
wl:arglist_info(u_yloop, f_u_yloop, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}).
wl: init_args(0, f_u_yloop).

/*

### Compiled:  `U::YLOOP` 
*/
f_u_yloop(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	set_var(GEnv, u_xx_loop_alist_xx, []),
	get_var(GEnv, u_clauses, Clauses_Get),
	cl_mapcar(eval, [Clauses_Get], Mapcar_Ret),
	f_u_iteration_variable_exists_p(IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	    f_u_substitute_iteration_variable(Xx_loop_alist_xx_Get, TrueResult),
	    set_var(GEnv, u_xx_loop_alist_xx, TrueResult),
	    _82997644=TrueResult
	;   _82997644=[]
	),
	f_u_fetch_clauses(do, Dos_Init),
	f_u_fetch_clauses(u_after, Afters_Init),
	f_u_fetch_clauses(u_end_test, End_tests_Init),
	f_u_fetch_clauses(u_initializations, Bindings_Init),
	f_u_fetch_clauses(u_result, Result_Init),
	f_u_fetch_clauses(u_next, Nexts_Init),
	f_u_fetch_clauses(u_before, Befores_Init),
	f_u_fetch_clauses(u_middle, Middle_stuff_Init),
	f_u_fetch_clauses(u_front, Front_stuff_Init),
	f_u_fetch_clauses(end, End_stuff_Init),
	cl_gensym(Block_label_Init),
	LEnv=[bv(u_dos, Dos_Init), bv(u_afters, Afters_Init), bv(u_end_tests, End_tests_Init), bv(bindings, Bindings_Init), bv(u_result, Result_Init), bv(u_nexts, Nexts_Init), bv(u_befores, Befores_Init), bv(u_middle_stuff, Middle_stuff_Init), bv(u_front_stuff, Front_stuff_Init), bv(u_end_stuff, End_stuff_Init), bv(u_block_label, Block_label_Init)|GEnv],
	set_var(LEnv, u_xx_loop_alist_xx, []),
	get_var(LEnv, bindings, Bindings_Get),
	get_var(LEnv, u_befores, Befores_Get),
	get_var(LEnv, u_block_label, Block_label_Get32),
	get_var(LEnv, u_end_tests, End_tests_Get),
	get_var(LEnv, u_front_stuff, Front_stuff_Get),
	(   get_var(LEnv, u_result, Result_Get),
	    Result_Get\==[],
	    CDR55=Result_Get
	->  true
	;   CDR55=[[]]
	),
	get_var(LEnv, u_dos, Dos_Get),
	get_var(LEnv, u_end_tests, End_tests_Get38),
	get_var(LEnv, u_middle_stuff, Middle_stuff_Get),
	get_var(LEnv, u_nexts, Nexts_Get),
	bq_append(Nexts_Get,
		  [[if, [or|End_tests_Get38], [], [go, loop]]],
		  Bq_append_Ret),
	bq_append(Middle_stuff_Get, Bq_append_Ret, Bq_append_Ret49),
	bq_append([loop|Dos_Get], Bq_append_Ret49, Bq_append_Ret50),
	get_var(LEnv, u_afters, Afters_Get),
	get_var(LEnv, u_block_label, Block_label_Get41),
	get_var(LEnv, u_end_stuff, End_stuff_Get),
	(   get_var(LEnv, u_result, Result_Get42),
	    Result_Get42\==[],
	    CDR=Result_Get42
	->  true
	;   CDR=[[]]
	),
	bq_append(End_stuff_Get,
		  [[return_from, Block_label_Get41|CDR]],
		  Bq_append_Ret51),
	bq_append([[tagbody|Bq_append_Ret50]|Afters_Get],
		  Bq_append_Ret51,
		  Bq_append_Ret53),
	bq_append(Front_stuff_Get,
		  
		  [ 
		    [ if,
		      [or|End_tests_Get],
		      [return_from, Block_label_Get32|CDR55]
		    ]
		  | Bq_append_Ret53
		  ],
		  Bq_append_Ret54),
	bq_append([Bindings_Get|Befores_Get], Bq_append_Ret54, Bq_append_Ret56),
	f_u_clear_loop_alist(Loop_alist_Ret),
	[unwind_protect, [block, Block_label_Get32, [let_xx|Bq_append_Ret56]], Loop_alist_Ret]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_yloop, classof, claz_macro),
   set_opv(u_yloop, compile_as, kw_operator),
   set_opv(u_yloop, function, f_u_yloop),
   _Ignored4=u_yloop.
/*
:- side_effect(assert_lsp(u_yloop,
			  wl:lambda_def(defmacro, u_yloop, f_u_yloop, [c38_rest, u_clauses], [progn, [setf, u_xx_loop_alist_xx, []], [mapcar, [quote, eval], u_clauses], [when, [u_iteration_variable_exists_p], [setf, u_xx_loop_alist_xx, [u_substitute_iteration_variable, u_xx_loop_alist_xx]]], [let, [[u_dos, [u_fetch_clauses, do]], [u_afters, [u_fetch_clauses, u_after]], [u_end_tests, [u_fetch_clauses, u_end_test]], [bindings, [u_fetch_clauses, u_initializations]], [u_result, [u_fetch_clauses, u_result]], [u_nexts, [u_fetch_clauses, u_next]], [u_befores, [u_fetch_clauses, u_before]], [u_middle_stuff, [u_fetch_clauses, u_middle]], [u_front_stuff, [u_fetch_clauses, u_front]], [u_end_stuff, [u_fetch_clauses, end]], [u_block_label, [gensym]]], [setf, u_xx_loop_alist_xx, []], ['#BQ', [unwind_protect, [block, ['#COMMA', u_block_label], [let_xx, [['#BQ-COMMA-ELIPSE', bindings]], ['#BQ-COMMA-ELIPSE', u_befores], ['#BQ-COMMA-ELIPSE', u_front_stuff], [if, [or, ['#BQ-COMMA-ELIPSE', u_end_tests]], [return_from, ['#COMMA', u_block_label], ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]]], [tagbody, loop, ['#BQ-COMMA-ELIPSE', u_dos], ['#BQ-COMMA-ELIPSE', u_middle_stuff], ['#BQ-COMMA-ELIPSE', u_nexts], [if, [or, ['#BQ-COMMA-ELIPSE', u_end_tests]], [], [go, loop]]], ['#BQ-COMMA-ELIPSE', u_afters], ['#BQ-COMMA-ELIPSE', u_end_stuff], [return_from, ['#COMMA', u_block_label], ['#BQ-COMMA-ELIPSE', [or, u_result, [quote, [[]]]]]]]], ['#COMMA', [u_clear_loop_alist]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_yloop,
			  wl:arglist_info(u_yloop, f_u_yloop, [c38_rest, u_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_clauses], opt:0, req:0, rest:[u_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_yloop, wl:init_args(0, f_u_yloop))).
*/
/*
; you have an iteration variuable to subsitute
*/
/*
; if there are multiple end-test's, accomidate them
*/
/*
; with an or wrapped around the end-test    
*/
/*
if you have nothing to do, jump -fr
*/
/*
; return results or nil              
*/
/*
(defmacro maximize (expression)
  (add-element-to-end-of-loop-alist `(maximum-variable ,expression) 
                                    'initializations) 
  (add-element-to-loop-alist
    `(if (> ,expression maximum-variable)
         (setf maximum-variable ,expression))
    'middle-stuff)
  (result maximum-variable)
  t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:16948 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,maximize,[expression],['add-element-to-end-of-loop-alist',['#BQ',['maximum-variable',['#COMMA',expression]]],[quote,initializations]],['add-element-to-loop-alist',['#BQ',[if,[>,['#COMMA',expression],'maximum-variable'],[setf,'maximum-variable',['#COMMA',expression]]]],[quote,'middle-stuff']],[result,'maximum-variable'],t])
wl:lambda_def(defmacro, u_maximize, f_u_maximize, [u_expression], [progn, [u_add_element_to_end_of_loop_alist, ['#BQ', [u_maximum_variable, ['#COMMA', u_expression]]], [quote, u_initializations]], [u_add_element_to_loop_alist, ['#BQ', [if, [>, ['#COMMA', u_expression], u_maximum_variable], [setf, u_maximum_variable, ['#COMMA', u_expression]]]], [quote, u_middle_stuff]], [u_result, u_maximum_variable], t]).
wl:arglist_info(u_maximize, f_u_maximize, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_maximize).

/*

### Compiled:  `U::MAXIMIZE` 
*/
f_u_maximize(Expression, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_expression, Expression)|Global_env_Ret],
	get_var(Env, u_expression, Expression_Get),
	f_u_add_element_to_end_of_loop_alist(
					     [ u_maximum_variable,
					       Expression_Get
					     ],
					     u_initializations,
					     Initializations),
	get_var(Env, u_expression, Expression_Get8),
	f_u_add_element_to_loop_alist(
				      [ if,
					[>, Expression_Get8, u_maximum_variable],
					
					[ setf,
					  u_maximum_variable,
					  Expression_Get8
					]
				      ],
				      u_middle_stuff,
				      Middle_stuff),
	get_var(Env, u_maximum_variable, Maximum_variable_Get),
	f_u_result(Maximum_variable_Get, Result_Ret),
	t=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_maximize, classof, claz_macro),
   set_opv(u_maximize, compile_as, kw_operator),
   set_opv(u_maximize, function, f_u_maximize),
   _Ignored4=u_maximize.
/*
:- side_effect(assert_lsp(u_maximize,
			  wl:lambda_def(defmacro, u_maximize, f_u_maximize, [u_expression], [progn, [u_add_element_to_end_of_loop_alist, ['#BQ', [u_maximum_variable, ['#COMMA', u_expression]]], [quote, u_initializations]], [u_add_element_to_loop_alist, ['#BQ', [if, [>, ['#COMMA', u_expression], u_maximum_variable], [setf, u_maximum_variable, ['#COMMA', u_expression]]]], [quote, u_middle_stuff]], [u_result, u_maximum_variable], t]))).
*/
/*
:- side_effect(assert_lsp(u_maximize,
			  wl:arglist_info(u_maximize, f_u_maximize, [u_expression], arginfo{all:[u_expression], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_expression], opt:0, req:[u_expression], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_maximize, wl:init_args(exact_only, f_u_maximize))).
*/
/*
(defun add-element-to-end-of-loop-alist (element clause-key)
  "Adds elements to a particular assoc sublist."
  (cond
    ((null (rassoc clause-key *loop-alist*))
     (record-in-loop-alist element clause-key))
    (t (rplaca (rassoc clause-key *loop-alist*)
               (reverse (cons element 
                              (car (rassoc clause-key *loop-alist*))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17274 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'add-element-to-end-of-loop-alist',[element,'clause-key'],'$STRING'("Adds elements to a particular assoc sublist."),[cond,[[null,[rassoc,'clause-key','*loop-alist*']],['record-in-loop-alist',element,'clause-key']],[t,[rplaca,[rassoc,'clause-key','*loop-alist*'],[reverse,[cons,element,[car,[rassoc,'clause-key','*loop-alist*']]]]]]]])
doc: doc_string(u_add_element_to_end_of_loop_alist,
	      _86577466,
	      function,
	      "Adds elements to a particular assoc sublist.").

wl:lambda_def(defun, u_add_element_to_end_of_loop_alist, f_u_add_element_to_end_of_loop_alist, [u_element, u_clause_key], [[cond, [[null, [rassoc, u_clause_key, u_xx_loop_alist_xx]], [u_record_in_loop_alist, u_element, u_clause_key]], [t, [rplaca, [rassoc, u_clause_key, u_xx_loop_alist_xx], [reverse, [cons, u_element, [car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]]]]]]]).
wl:arglist_info(u_add_element_to_end_of_loop_alist, f_u_add_element_to_end_of_loop_alist, [u_element, u_clause_key], arginfo{all:[u_element, u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, u_clause_key], opt:0, req:[u_element, u_clause_key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_add_element_to_end_of_loop_alist).

/*

### Compiled:  `U::ADD-ELEMENT-TO-END-OF-LOOP-ALIST` 
*/
f_u_add_element_to_end_of_loop_alist(Element, Clause_key, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_element, Element), bv(u_clause_key, Clause_key)|Env],
	get_var(Env22, u_clause_key, Clause_key_Get),
	get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get),
	cl_rassoc(Clause_key_Get, Xx_loop_alist_xx_Get, [], IFTEST),
	(   IFTEST==[]
	->  get_var(Env22, u_clause_key, Clause_key_Get12),
	    get_var(Env22, u_element, Element_Get),
	    f_u_record_in_loop_alist(Element_Get, Clause_key_Get12, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env22, u_clause_key, Clause_key_Get13),
	    get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get14),
	    cl_rassoc(Clause_key_Get13, Xx_loop_alist_xx_Get14, [], Rplaca_Param),
	    get_var(Env22, u_clause_key, Clause_key_Get16),
	    get_var(Env22, u_element, Element_Get15),
	    get_var(Env22, u_xx_loop_alist_xx, Xx_loop_alist_xx_Get17),
	    cl_rassoc(Clause_key_Get16, Xx_loop_alist_xx_Get17, [], Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    Reverse_Param=[Element_Get15|Car_Ret],
	    cl_reverse(Reverse_Param, Reverse_Ret),
	    cl_rplaca(Rplaca_Param, Reverse_Ret, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_element_to_end_of_loop_alist, classof, claz_function),
   set_opv(u_add_element_to_end_of_loop_alist, compile_as, kw_function),
   set_opv(u_add_element_to_end_of_loop_alist,
	   function,
	   f_u_add_element_to_end_of_loop_alist),
   _Ignored4=u_add_element_to_end_of_loop_alist.
/*
:- side_effect(assert_lsp(u_add_element_to_end_of_loop_alist,
			  doc:doc_string(u_add_element_to_end_of_loop_alist, _86577466, function, "Adds elements to a particular assoc sublist."))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_end_of_loop_alist,
			  wl:lambda_def(defun, u_add_element_to_end_of_loop_alist, f_u_add_element_to_end_of_loop_alist, [u_element, u_clause_key], [[cond, [[null, [rassoc, u_clause_key, u_xx_loop_alist_xx]], [u_record_in_loop_alist, u_element, u_clause_key]], [t, [rplaca, [rassoc, u_clause_key, u_xx_loop_alist_xx], [reverse, [cons, u_element, [car, [rassoc, u_clause_key, u_xx_loop_alist_xx]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_end_of_loop_alist,
			  wl:arglist_info(u_add_element_to_end_of_loop_alist, f_u_add_element_to_end_of_loop_alist, [u_element, u_clause_key], arginfo{all:[u_element, u_clause_key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_element, u_clause_key], opt:0, req:[u_element, u_clause_key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_element_to_end_of_loop_alist,
			  wl:init_args(exact_only, f_u_add_element_to_end_of_loop_alist))).
*/
/*
(defun define-and-rename-loop-locals (where-to-add arg-list result body)
  (when arg-list
    (dolist (clause arg-list)
      (let* (
             (var nil)
             (new-var (gensym)))
        (if (listp clause) (setq var (car clause)) (setq var clause))
            ;; nsubst doesnt work on body as it isn't quite represented 
            ;; as a list on the function stack
        (setf arg-list (subst new-var var arg-list))
        (setf body (subst new-var var body))
        (setf result (subst new-var var result)))))
  
  (if result (add-element-to-loop-alist (cons 'progn (list result)) 'result))
  (add-element-to-loop-alist  body where-to-add)
  (when arg-list
    (dolist (new-var arg-list)
    (add-element-to-loop-alist new-var 'initializations))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:17648 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'define-and-rename-loop-locals',['where-to-add','arg-list',result,body],[when,'arg-list',[dolist,[clause,'arg-list'],['let*',[[var,[]],['new-var',[gensym]]],[if,[listp,clause],[setq,var,[car,clause]],[setq,var,clause]],[setf,'arg-list',[subst,'new-var',var,'arg-list']],[setf,body,[subst,'new-var',var,body]],[setf,result,[subst,'new-var',var,result]]]]],[if,result,['add-element-to-loop-alist',[cons,[quote,progn],[list,result]],[quote,result]]],['add-element-to-loop-alist',body,'where-to-add'],[when,'arg-list',[dolist,['new-var','arg-list'],['add-element-to-loop-alist','new-var',[quote,initializations]]]]])
wl:lambda_def(defun, u_define_and_rename_loop_locals, f_u_define_and_rename_loop_locals, [u_where_to_add, u_arg_list, u_result, u_body], [[when, u_arg_list, [dolist, [u_clause, u_arg_list], [let_xx, [[u_var, []], [u_new_var, [gensym]]], [if, [listp, u_clause], [setq, u_var, [car, u_clause]], [setq, u_var, u_clause]], [setf, u_arg_list, [subst, u_new_var, u_var, u_arg_list]], [setf, u_body, [subst, u_new_var, u_var, u_body]], [setf, u_result, [subst, u_new_var, u_var, u_result]]]]], [if, u_result, [u_add_element_to_loop_alist, [cons, [quote, progn], [list, u_result]], [quote, u_result]]], [u_add_element_to_loop_alist, u_body, u_where_to_add], [when, u_arg_list, [dolist, [u_new_var, u_arg_list], [u_add_element_to_loop_alist, u_new_var, [quote, u_initializations]]]]]).
wl:arglist_info(u_define_and_rename_loop_locals, f_u_define_and_rename_loop_locals, [u_where_to_add, u_arg_list, u_result, u_body], arginfo{all:[u_where_to_add, u_arg_list, u_result, u_body], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_where_to_add, u_arg_list, u_result, u_body], opt:0, req:[u_where_to_add, u_arg_list, u_result, u_body], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_define_and_rename_loop_locals).

/*

### Compiled:  `U::DEFINE-AND-RENAME-LOOP-LOCALS` 
*/
f_u_define_and_rename_loop_locals(Where_to_add, Arg_list, Result60, Body, FnResult) :-
	nop(global_env(Env)),
	Env57=[bv(u_where_to_add, Where_to_add), bv(u_arg_list, Arg_list), bv(u_result, Result60), bv(u_body, Body)|Env],
	get_var(Env57, u_arg_list, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env57, u_arg_list, Arg_list_Get10),
	    BV=bv(u_clause, Ele),
	    Env2=[BV|Env57],
	    forall(member(Ele, Arg_list_Get10),
		   ( nb_setarg(2, BV, Ele),
		     cl_gensym(New_var_Init),
		     LEnv=[bv(u_var, []), bv(u_new_var, New_var_Init)|Env2],
		     get_var(LEnv, u_clause, Clause_Get),
		     (   is_listp(Clause_Get)
		     ->  get_var(LEnv, u_clause, Clause_Get20),
			 cl_car(Clause_Get20, TrueResult),
			 set_var(LEnv, u_var, TrueResult),
			 _87923062=TrueResult
		     ;   get_var(LEnv, u_clause, Clause_Get21),
			 set_var(LEnv, u_var, Clause_Get21),
			 _87923062=Clause_Get21
		     ),
		     get_var(LEnv, u_arg_list, Arg_list_Get26),
		     get_var(LEnv, u_new_var, New_var_Get),
		     get_var(LEnv, u_var, Var_Get),
		     cl_subst(New_var_Get, Var_Get, Arg_list_Get26, Arg_list62),
		     set_var(LEnv, u_arg_list, Arg_list62),
		     get_var(LEnv, u_body, Body_Get),
		     get_var(LEnv, u_new_var, New_var_Get27),
		     get_var(LEnv, u_var, Var_Get28),
		     cl_subst(New_var_Get27, Var_Get28, Body_Get, Body63),
		     set_var(LEnv, u_body, Body63),
		     get_var(LEnv, u_new_var, New_var_Get30),
		     get_var(LEnv, u_result, Result_Get),
		     get_var(LEnv, u_var, Var_Get31),
		     cl_subst(New_var_Get30, Var_Get31, Result_Get, LetResult),
		     set_var(LEnv, u_result, LetResult)
		   )),
	    _87901790=LetResult
	;   _87901790=[]
	),
	get_var(Env57, u_result, IFTEST38),
	(   IFTEST38\==[]
	->  get_var(Env57, u_result, Result_Get41),
	    CDR=[Result_Get41],
	    Loop_alist_Param=[progn|CDR],
	    f_u_add_element_to_loop_alist(Loop_alist_Param,
					  u_result,
					  TrueResult42),
	    _88235700=TrueResult42
	;   _88235700=[]
	),
	get_var(Env57, u_body, Body_Get43),
	get_var(Env57, u_where_to_add, Where_to_add_Get),
	f_u_add_element_to_loop_alist(Body_Get43,
				      Where_to_add_Get,
				      Loop_alist_Ret),
	get_var(Env57, u_arg_list, IFTEST45),
	(   IFTEST45\==[]
	->  get_var(Env57, u_arg_list, Arg_list_Get48),
	    BV50=bv(u_new_var, Ele52),
	    Env251=[BV50|Env57],
	    forall(member(Ele52, Arg_list_Get48),
		   ( nb_setarg(2, BV50, Ele52),
		     get_var(Env251, u_new_var, New_var_Get49),
		     f_u_add_element_to_loop_alist(New_var_Get49,
						   u_initializations,
						   TrueResult54)
		   )),
	    FnResult=TrueResult54
	;   FnResult=[]
	).
:- set_opv(f_u_define_and_rename_loop_locals, classof, claz_function),
   set_opv(u_define_and_rename_loop_locals, compile_as, kw_function),
   set_opv(u_define_and_rename_loop_locals,
	   function,
	   f_u_define_and_rename_loop_locals),
   _Ignored4=u_define_and_rename_loop_locals.
/*
:- side_effect(assert_lsp(u_define_and_rename_loop_locals,
			  wl:lambda_def(defun, u_define_and_rename_loop_locals, f_u_define_and_rename_loop_locals, [u_where_to_add, u_arg_list, u_result, u_body], [[when, u_arg_list, [dolist, [u_clause, u_arg_list], [let_xx, [[u_var, []], [u_new_var, [gensym]]], [if, [listp, u_clause], [setq, u_var, [car, u_clause]], [setq, u_var, u_clause]], [setf, u_arg_list, [subst, u_new_var, u_var, u_arg_list]], [setf, u_body, [subst, u_new_var, u_var, u_body]], [setf, u_result, [subst, u_new_var, u_var, u_result]]]]], [if, u_result, [u_add_element_to_loop_alist, [cons, [quote, progn], [list, u_result]], [quote, u_result]]], [u_add_element_to_loop_alist, u_body, u_where_to_add], [when, u_arg_list, [dolist, [u_new_var, u_arg_list], [u_add_element_to_loop_alist, u_new_var, [quote, u_initializations]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_define_and_rename_loop_locals,
			  wl:arglist_info(u_define_and_rename_loop_locals, f_u_define_and_rename_loop_locals, [u_where_to_add, u_arg_list, u_result, u_body], arginfo{all:[u_where_to_add, u_arg_list, u_result, u_body], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_where_to_add, u_arg_list, u_result, u_body], opt:0, req:[u_where_to_add, u_arg_list, u_result, u_body], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_define_and_rename_loop_locals,
			  wl:init_args(exact_only, f_u_define_and_rename_loop_locals))).
*/
/*
; nsubst doesnt work on body as it isn't quite represented 
*/
/*
; as a list on the function stack
*/
/*
(defmacro add-to-loop-macro (where-to-add arg-list result &body body)  
  `(define-and-rename-loop-locals ',where-to-add ',arg-list ',result ',@body))

;;;     Examples of "programmer" defined loop macro functions. They are to
;;; function as their zetalisp loop counterparts. To define a yloop macro
;;; you must invoke the macro ADD-TO-LOOP-MACRO. This macro: 1)substitutes
;;; symbols (via gensym) so as to avoid symbol conflicts within the loop
;;; in the future; 2) provides requested local loop variables that will be
;;; within the lexical scope of the repeating statements (i.e. the loop),
;;; and ; 3)places the new code in the requested part of the
;;; loop. (Specifically the yloop macro is conceptually separated into 3
;;; parts: the FRONT, the MIDDLE and the end. Code that is in the FRONT of
;;; the yloop macro is executed after local bindings are made but before
;;; the executions of the statements to be repeated. Code that is in the
;;; MIDDLE of the yloop macro is executed after the FRONT code has been
;;; executed and is executed repeatedly until some termination condition
;;; is met. Code in the END of the yloop macro is executed after the loop
;;; terminates normally.)  The first argument to ADD-TO-LOOP-MACRO is to
;;; indicate where to place the new code.  It is to be one of FRONT MIDDLE
;;; END. The second argument is a list of desired local yloop
;;; variables. The syntax is to be the same as the car of let statements
;;; as that list will actually be placed at the position of the first
;;; argument in the let statement. The third argument is the variable
;;; which will given to the (return ) statement of the loop so that its
;;; value will be returned on normal termination of the loop. And the
;;; final arguments are to be the body of new macro to be inserted in the
;;; loop .
;;; 
;;; Hint When you want something returned, declare a new local loop
;;; variable, declare it as that which will be returned and set your
;;; answer to it.

;(defmacro sum (expression)
;  `(add-to-loop-macro middle ((sum 0)) sum
;    (setq sum (+ sum ,expression))))

;(defmacro ywhen (test &body clauses-to-execute)
;  `(add-to-loop-macro middle nil nil
;                      (when ,test ,@clauses-to-execute)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:18417 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'add-to-loop-macro',['where-to-add','arg-list',result,'&body',body],['#BQ',['define-and-rename-loop-locals',[quote,['#COMMA','where-to-add']],[quote,['#COMMA','arg-list']],[quote,['#COMMA',result]],[quote,['#BQ-COMMA-ELIPSE',body]]]]])
wl:lambda_def(defmacro, u_add_to_loop_macro, f_u_add_to_loop_macro, [u_where_to_add, u_arg_list, u_result, c38_body, u_body], [progn, ['#BQ', [u_define_and_rename_loop_locals, [quote, ['#COMMA', u_where_to_add]], [quote, ['#COMMA', u_arg_list]], [quote, ['#COMMA', u_result]], [quote, ['#BQ-COMMA-ELIPSE', u_body]]]]]).
wl:arglist_info(u_add_to_loop_macro, f_u_add_to_loop_macro, [u_where_to_add, u_arg_list, u_result, c38_body, u_body], arginfo{all:[u_where_to_add, u_arg_list, u_result], allow_other_keys:0, aux:0, body:[u_body], complex:[body], env:0, key:0, names:[u_where_to_add, u_arg_list, u_result, u_body], opt:0, req:[u_where_to_add, u_arg_list, u_result], rest:[u_body], sublists:0, whole:0}).
wl: init_args(3, f_u_add_to_loop_macro).

/*

### Compiled:  `U::ADD-TO-LOOP-MACRO` 
*/
f_u_add_to_loop_macro(Where_to_add, Arg_list, Result, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_where_to_add, Where_to_add), bv(u_arg_list, Arg_list), bv(u_result, Result), bv(u_body, Body)]|Global_env_Ret]|Global_env_Ret],
	as_body(u_body, Body, RestNKeys),
	get_var(GEnv, u_arg_list, Arg_list_Get),
	get_var(GEnv, u_body, Body_Get),
	get_var(GEnv, u_result, Result_Get),
	get_var(GEnv, u_where_to_add, Where_to_add_Get),
	[u_define_and_rename_loop_locals, [quote, Where_to_add_Get], [quote, Arg_list_Get], [quote, Result_Get], [quote|Body_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_add_to_loop_macro, classof, claz_macro),
   set_opv(u_add_to_loop_macro, compile_as, kw_operator),
   set_opv(u_add_to_loop_macro, function, f_u_add_to_loop_macro),
   _Ignored4=u_add_to_loop_macro.
/*
:- side_effect(assert_lsp(u_add_to_loop_macro,
			  wl:lambda_def(defmacro, u_add_to_loop_macro, f_u_add_to_loop_macro, [u_where_to_add, u_arg_list, u_result, c38_body, u_body], [progn, ['#BQ', [u_define_and_rename_loop_locals, [quote, ['#COMMA', u_where_to_add]], [quote, ['#COMMA', u_arg_list]], [quote, ['#COMMA', u_result]], [quote, ['#BQ-COMMA-ELIPSE', u_body]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_add_to_loop_macro,
			  wl:arglist_info(u_add_to_loop_macro, f_u_add_to_loop_macro, [u_where_to_add, u_arg_list, u_result, c38_body, u_body], arginfo{all:[u_where_to_add, u_arg_list, u_result], allow_other_keys:0, aux:0, body:[u_body], complex:[body], env:0, key:0, names:[u_where_to_add, u_arg_list, u_result, u_body], opt:0, req:[u_where_to_add, u_arg_list, u_result], rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_add_to_loop_macro,
			  wl:init_args(3, f_u_add_to_loop_macro))).
*/
/*
;;     Examples of "programmer" defined loop macro functions. They are to
*/
/*
;; function as their zetalisp loop counterparts. To define a yloop macro
*/
/*
;; you must invoke the macro ADD-TO-LOOP-MACRO. This macro: 1)substitutes
*/
/*
;; symbols (via gensym) so as to avoid symbol conflicts within the loop
*/
/*
;; in the future; 2) provides requested local loop variables that will be
*/
/*
;; within the lexical scope of the repeating statements (i.e. the loop),
*/
/*
;; and ; 3)places the new code in the requested part of the
*/
/*
;; loop. (Specifically the yloop macro is conceptually separated into 3
*/
/*
;; parts: the FRONT, the MIDDLE and the end. Code that is in the FRONT of
*/
/*
;; the yloop macro is executed after local bindings are made but before
*/
/*
;; the executions of the statements to be repeated. Code that is in the
*/
/*
;; MIDDLE of the yloop macro is executed after the FRONT code has been
*/
/*
;; executed and is executed repeatedly until some termination condition
*/
/*
;; is met. Code in the END of the yloop macro is executed after the loop
*/
/*
;; terminates normally.)  The first argument to ADD-TO-LOOP-MACRO is to
*/
/*
;; indicate where to place the new code.  It is to be one of FRONT MIDDLE
*/
/*
;; END. The second argument is a list of desired local yloop
*/
/*
;; variables. The syntax is to be the same as the car of let statements
*/
/*
;; as that list will actually be placed at the position of the first
*/
/*
;; argument in the let statement. The third argument is the variable
*/
/*
;; which will given to the (return ) statement of the loop so that its
*/
/*
;; value will be returned on normal termination of the loop. And the
*/
/*
;; final arguments are to be the body of new macro to be inserted in the
*/
/*
;; loop .
*/
/*
;; 
*/
/*
;; Hint When you want something returned, declare a new local loop
*/
/*
;; variable, declare it as that which will be returned and set your
*/
/*
;; answer to it.
*/
/*
(defmacro sum (expression)
*/
/*
  `(add-to-loop-macro middle ((sum 0)) sum
*/
/*
    (setq sum (+ sum ,expression))))
*/
/*
(defmacro ywhen (test &body clauses-to-execute)
*/
/*
  `(add-to-loop-macro middle nil nil
*/
/*
                      (when ,test ,@clauses-to-execute)))
*/
/*
(defun find-form (sequence form-to-find)
  (cond
    ((atom sequence) nil)
    ((null sequence) nil)
    ((equal (car sequence) form-to-find) sequence)
    (t (list-without-nils
         (find-form (car sequence) form-to-find)
         (find-form (cdr sequence) form-to-find)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:20658 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'find-form',[sequence,'form-to-find'],[cond,[[atom,sequence],[]],[[null,sequence],[]],[[equal,[car,sequence],'form-to-find'],sequence],[t,['list-without-nils',['find-form',[car,sequence],'form-to-find'],['find-form',[cdr,sequence],'form-to-find']]]]])
wl:lambda_def(defun, u_find_form, f_u_find_form, [sequence, u_form_to_find], [[cond, [[atom, sequence], []], [[null, sequence], []], [[equal, [car, sequence], u_form_to_find], sequence], [t, [u_list_without_nils, [u_find_form, [car, sequence], u_form_to_find], [u_find_form, [cdr, sequence], u_form_to_find]]]]]).
wl:arglist_info(u_find_form, f_u_find_form, [sequence, u_form_to_find], arginfo{all:[sequence, u_form_to_find], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, u_form_to_find], opt:0, req:[sequence, u_form_to_find], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_find_form).

/*

### Compiled:  `U::FIND-FORM` 
*/
f_u_find_form(Sequence, Form_to_find, ElseResult27) :-
	nop(global_env(Env)),
	Env31=[bv(sequence, Sequence), bv(u_form_to_find, Form_to_find)|Env],
	get_var(Env31, sequence, Sequence_Get),
	(   Sequence_Get\=[CAR|CDR]
	->  ElseResult27=[]
	;   get_var(Env31, sequence, IFTEST11),
	    (   IFTEST11==[]
	    ->  ElseResult27=[]
	    ;   get_var(Env31, sequence, Sequence_Get15),
		cl_car(Sequence_Get15, PredArg1Result),
		get_var(Env31, u_form_to_find, Form_to_find_Get),
		(   is_equal(PredArg1Result, Form_to_find_Get)
		->  get_var(Env31, sequence, Sequence_Get20),
		    ElseResult27=Sequence_Get20
		;   get_var(Env31, sequence, Sequence_Get21),
		    cl_car(Sequence_Get21, Find_form_Param),
		    get_var(Env31, u_form_to_find, Form_to_find_Get22),
		    f_u_find_form(Find_form_Param,
				  Form_to_find_Get22,
				  Without_nils_Param),
		    get_var(Env31, sequence, Sequence_Get23),
		    cl_cdr(Sequence_Get23, Find_form_Param35),
		    get_var(Env31, u_form_to_find, Form_to_find_Get24),
		    f_u_find_form(Find_form_Param35,
				  Form_to_find_Get24,
				  Find_form_Ret),
		    f_u_list_without_nils(Without_nils_Param,
					  Find_form_Ret,
					  ElseResult),
		    ElseResult27=ElseResult
		)
	    )
	).
:- set_opv(f_u_find_form, classof, claz_function),
   set_opv(u_find_form, compile_as, kw_function),
   set_opv(u_find_form, function, f_u_find_form),
   _Ignored4=u_find_form.
/*
:- side_effect(assert_lsp(u_find_form,
			  wl:lambda_def(defun, u_find_form, f_u_find_form, [sequence, u_form_to_find], [[cond, [[atom, sequence], []], [[null, sequence], []], [[equal, [car, sequence], u_form_to_find], sequence], [t, [u_list_without_nils, [u_find_form, [car, sequence], u_form_to_find], [u_find_form, [cdr, sequence], u_form_to_find]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_find_form,
			  wl:arglist_info(u_find_form, f_u_find_form, [sequence, u_form_to_find], arginfo{all:[sequence, u_form_to_find], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, u_form_to_find], opt:0, req:[sequence, u_form_to_find], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_find_form, wl:init_args(exact_only, f_u_find_form))).
*/
/*
(defun substitute-loop-return (label lisp-expressions)
  (dolist (subst-clause (find-form lisp-expressions 'loop-return))
    (nsublis `(,subst-clause  (return-from ,label (cdr subst-clause)))
     lisp-expressions)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:20939 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'substitute-loop-return',[label,'lisp-expressions'],[dolist,['subst-clause',['find-form','lisp-expressions',[quote,'loop-return']]],[nsublis,['#BQ',[['#COMMA','subst-clause'],['return-from',['#COMMA',label],[cdr,'subst-clause']]]],'lisp-expressions']]])
wl:lambda_def(defun, u_substitute_loop_return, f_u_substitute_loop_return, [u_label, u_lisp_expressions], [[dolist, [u_subst_clause, [u_find_form, u_lisp_expressions, [quote, u_loop_return]]], [nsublis, ['#BQ', [['#COMMA', u_subst_clause], [return_from, ['#COMMA', u_label], [cdr, u_subst_clause]]]], u_lisp_expressions]]]).
wl:arglist_info(u_substitute_loop_return, f_u_substitute_loop_return, [u_label, u_lisp_expressions], arginfo{all:[u_label, u_lisp_expressions], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_label, u_lisp_expressions], opt:0, req:[u_label, u_lisp_expressions], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_substitute_loop_return).

/*

### Compiled:  `U::SUBSTITUTE-LOOP-RETURN` 
*/
f_u_substitute_loop_return(Label, Lisp_expressions, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(u_label, Label), bv(u_lisp_expressions, Lisp_expressions)|Env],
	get_var(Env17, u_lisp_expressions, Lisp_expressions_Get),
	f_u_find_form(Lisp_expressions_Get, u_loop_return, List),
	BV=bv(u_subst_clause, Ele),
	Env2=[BV|Env17],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Env2, u_label, Label_Get),
		 get_var(Env2, u_lisp_expressions, Lisp_expressions_Get10),
		 get_var(Env2, u_subst_clause, Subst_clause_Get),
		 cl_nsublis(
			    [ Subst_clause_Get,
			      [return_from, Label_Get, [cdr, u_subst_clause]]
			    ],
			    Lisp_expressions_Get10,
			    Nsublis_Ret)
	       )),
	Nsublis_Ret=FnResult.
:- set_opv(f_u_substitute_loop_return, classof, claz_function),
   set_opv(u_substitute_loop_return, compile_as, kw_function),
   set_opv(u_substitute_loop_return, function, f_u_substitute_loop_return),
   _Ignored4=u_substitute_loop_return.
/*
:- side_effect(assert_lsp(u_substitute_loop_return,
			  wl:lambda_def(defun, u_substitute_loop_return, f_u_substitute_loop_return, [u_label, u_lisp_expressions], [[dolist, [u_subst_clause, [u_find_form, u_lisp_expressions, [quote, u_loop_return]]], [nsublis, ['#BQ', [['#COMMA', u_subst_clause], [return_from, ['#COMMA', u_label], [cdr, u_subst_clause]]]], u_lisp_expressions]]]))).
*/
/*
:- side_effect(assert_lsp(u_substitute_loop_return,
			  wl:arglist_info(u_substitute_loop_return, f_u_substitute_loop_return, [u_label, u_lisp_expressions], arginfo{all:[u_label, u_lisp_expressions], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_label, u_lisp_expressions], opt:0, req:[u_label, u_lisp_expressions], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_substitute_loop_return,
			  wl:init_args(exact_only, f_u_substitute_loop_return))).
*/
/*
(defun list-without-nils (a b)
  (cond ((or (null a) (null b)) (append a b))
        (t (list a b))))

;;; *EOF*

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl:21158 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'list-without-nils',[a,b],[cond,[[or,[null,a],[null,b]],[append,a,b]],[t,[list,a,b]]]])
wl:lambda_def(defun, u_list_without_nils, f_u_list_without_nils, [u_a, u_b], [[cond, [[or, [null, u_a], [null, u_b]], [append, u_a, u_b]], [t, [list, u_a, u_b]]]]).
wl:arglist_info(u_list_without_nils, f_u_list_without_nils, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_list_without_nils).

/*

### Compiled:  `U::LIST-WITHOUT-NILS` 
*/
f_u_list_without_nils(A, B, FnResult) :-
	nop(global_env(Env)),
	Env20=[bv(u_a, A), bv(u_b, B)|Env],
	(   get_var(Env20, u_a, A_Get),
	    cl_null(A_Get, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(Env20, u_b, B_Get),
	    cl_null(B_Get, Null_Ret),
	    IFTEST=Null_Ret
	),
	(   IFTEST\==[]
	->  get_var(Env20, u_a, A_Get12),
	    get_var(Env20, u_b, B_Get13),
	    cl_append(A_Get12, B_Get13, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env20, u_a, A_Get14),
	    get_var(Env20, u_b, B_Get15),
	    FnResult=[A_Get14, B_Get15]
	).
:- set_opv(f_u_list_without_nils, classof, claz_function),
   set_opv(u_list_without_nils, compile_as, kw_function),
   set_opv(u_list_without_nils, function, f_u_list_without_nils),
   _Ignored4=u_list_without_nils.
/*
:- side_effect(assert_lsp(u_list_without_nils,
			  wl:lambda_def(defun, u_list_without_nils, f_u_list_without_nils, [u_a, u_b], [[cond, [[or, [null, u_a], [null, u_b]], [append, u_a, u_b]], [t, [list, u_a, u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_list_without_nils,
			  wl:arglist_info(u_list_without_nils, f_u_list_without_nils, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_list_without_nils,
			  wl:init_args(exact_only, f_u_list_without_nils))).
*/
/*
;; *EOF*
*/
/*
; *EOF*
*/


%; Total compilation time: 8.557 seconds


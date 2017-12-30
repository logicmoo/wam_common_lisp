#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Wed Dec 27 22:03:31 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(in-package #:system)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:system'])
:- cl_in_package(system1, _Ignored).
/*
(defpackage "SYSTEM" (:nicknames "SYS"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:24 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("SYSTEM"),[':nicknames','$STRING'("SYS")]])
:- cl_defpackage('$ARRAY'([*], claz_base_character, "SYSTEM"),
		 [kw_nicknames, '$ARRAY'([*], claz_base_character, "SYS")],
		 _Ignored).
/*
:- side_effect(add_opv_new(pkg_system, nicknames, "SYS")).
*/
/*
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:65 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("COMMON-LISP"),[':nicknames','$STRING'("CL"),'$STRING'("LISP")],[':uses','$STRING'("SYSTEM")]])
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "CL"),
		     '$ARRAY'([*], claz_base_character, "LISP")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "SYSTEM")]
		 ],
		 _Ignored).
/*
:- side_effect(add_opv_new(pkg_common_lisp, nicknames, "CL")).
*/
/*
:- side_effect(add_opv_new(pkg_common_lisp, nicknames, "LISP")).
*/
/*
:- side_effect(add_opv_new(pkg_common_lisp, uses, "SYSTEM")).
*/
/*
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:133 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("COMMON-LISP-USER"),[':nicknames','$STRING'("U"),'$STRING'("USER"),'$STRING'("CL-USER")],[':uses','$STRING'("COMMON-LISP")]])
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP-USER"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "U"),
		     '$ARRAY'([*], claz_base_character, "USER"),
		     '$ARRAY'([*], claz_base_character, "CL-USER")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "COMMON-LISP")]
		 ],
		 _Ignored).
/*
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "U")).
*/
/*
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "USER")).
*/
/*
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "CL-USER")).
*/
/*
:- side_effect(add_opv_new(pkg_common_lisp_user, uses, "COMMON-LISP")).
*/
/*
(defvar *lisp-file-type* "lisp") 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:221 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*lisp-file-type*','$STRING'("lisp")])
:- set_var(AEnv,
	   sys_xx_lisp_file_type_xx,
	   '$ARRAY'([*], claz_base_character, "lisp")).
/*
(defvar *default-pathname-defaults* #P"")
 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:255 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*default-pathname-defaults*','$OBJ'(claz_pathname,"")])
:- set_var(AEnv, xx_default_pathname_defaults_xx, '$OBJ'(claz_pathname, "")).
/*
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:299 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,dd,[],[let,[['*lisp-file-type*','$STRING'("cl")],['*default-pathname-defaults*',['merge-pathnames','$STRING'("daydreamer/")]]],[load,'$STRING'("dd")]]])
wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]).
wl:arglist_info(sys_dd, f_sys_dd, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_dd).

/*

### Compiled:  `SYS::DD` 
*/
f_sys_dd(FnResult) :-
	CDR=Env,
	global_env(Env),
	cl_merge_pathnames('$ARRAY'([*], claz_base_character, "daydreamer/"),
			   Xx_default_pathname_defaults_xx_Init),
	LEnv=[bv(sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl"))|CDR],
	save_special(sv(xx_default_pathname_defaults_xx,
			Xx_default_pathname_defaults_xx_Init,
			value,
			Value)),
	cl_load('$ARRAY'([*], claz_base_character, "dd"), [], LetResult),
	restore_special(sv(xx_default_pathname_defaults_xx,
			   Xx_default_pathname_defaults_xx_Init,
			   value,
			   Value)),
	LetResult=FnResult.
:- set_opv(f_sys_dd, classof, claz_function),
   set_opv(sys_dd, compile_as, kw_function),
   set_opv(sys_dd, function, f_sys_dd),
   DefunResult=sys_dd.
/*
:- side_effect(assert_lsp(sys_dd,
			  lambda_def(defun,
				     sys_dd,
				     f_sys_dd,
				     [],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_xx_lisp_file_type_xx,
					     '$ARRAY'([*],
						      claz_base_character,
						      "cl")
					   ],
					   
					   [ xx_default_pathname_defaults_xx,
					     
					     [ merge_pathnames,
					       '$ARRAY'([*],
							claz_base_character,
							"daydreamer/")
					     ]
					   ]
					 ],
					 
					 [ load,
					   '$ARRAY'([*],
						    claz_base_character,
						    "dd")
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dd,
			  arglist_info(sys_dd,
				       f_sys_dd,
				       [],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[],
						opt:0,
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_dd, init_args(exact_only, f_sys_dd))).
*/
/*
(defun show-ascii-art ()
        
(write-line "  __________    ")
(write-line " / ___  ___ \\   ")
(write-line "/ / @ \\/ @ \\ \\  ")
(write-line "\\ \\___/\\___/ /\\ ")
(write-line " \\____\\/____/|| ")
(write-line " /     /\\\\\\\\\\// ")
(write-line "|     |\\\\\\\\\\\\   ")
(write-line " \\      \\\\\\\\\\\\  ")
(write-line "   \\______/\\\\\\\\ ")
(write-line "    _||_||_     ")
(write-line "                "))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:432 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'show-ascii-art',[],['write-line','$STRING'("  __________    ")],['write-line','$STRING'(" / ___  ___ \\   ")],['write-line','$STRING'("/ / @ \\/ @ \\ \\  ")],['write-line','$STRING'("\\ \\___/\\___/ /\\ ")],['write-line','$STRING'(" \\____\\/____/|| ")],['write-line','$STRING'(" /     /\\\\\\\\\\// ")],['write-line','$STRING'("|     |\\\\\\\\\\\\   ")],['write-line','$STRING'(" \\      \\\\\\\\\\\\  ")],['write-line','$STRING'("   \\______/\\\\\\\\ ")],['write-line','$STRING'("    _||_||_     ")],['write-line','$STRING'("                ")]])
wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]).
wl:arglist_info(sys_show_ascii_art, f_sys_show_ascii_art, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_show_ascii_art).

/*

### Compiled:  `SYS::SHOW-ASCII-ART` 
*/
f_sys_show_ascii_art(FnResult) :-
	_2240204=Env,
	global_env(Env),
	cl_write_line('$ARRAY'([*], claz_base_character, "  __________    "),
		      Write_line_Ret),
	cl_write_line('$ARRAY'([*], claz_base_character, " / ___  ___ \\   "),
		      Write_line_Ret9),
	cl_write_line('$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  "),
		      Write_line_Ret10),
	cl_write_line('$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ "),
		      Write_line_Ret11),
	cl_write_line('$ARRAY'([*], claz_base_character, " \\____\\/____/|| "),
		      Write_line_Ret12),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " /     /\\\\\\\\\\// "),
		      Write_line_Ret13),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "|     |\\\\\\\\\\\\   "),
		      Write_line_Ret14),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " \\      \\\\\\\\\\\\  "),
		      Write_line_Ret15),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "   \\______/\\\\\\\\ "),
		      Write_line_Ret16),
	cl_write_line('$ARRAY'([*], claz_base_character, "    _||_||_     "),
		      Write_line_Ret17),
	cl_write_line('$ARRAY'([*], claz_base_character, "                "),
		      Write_line_Ret18),
	Write_line_Ret18=FnResult.
:- set_opv(f_sys_show_ascii_art, classof, claz_function),
   set_opv(sys_show_ascii_art, compile_as, kw_function),
   set_opv(sys_show_ascii_art, function, f_sys_show_ascii_art),
   DefunResult=sys_show_ascii_art.
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  lambda_def(defun,
				     sys_show_ascii_art,
				     f_sys_show_ascii_art,
				     [],
				     
				     [ 
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "  __________    ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " / ___  ___ \\   ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "/ / @ \\/ @ \\ \\  ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "\\ \\___/\\___/ /\\ ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " \\____\\/____/|| ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " /     /\\\\\\\\\\// ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "|     |\\\\\\\\\\\\   ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " \\      \\\\\\\\\\\\  ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "   \\______/\\\\\\\\ ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "    _||_||_     ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "                ")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  arglist_info(sys_show_ascii_art,
				       f_sys_show_ascii_art,
				       [],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[],
						opt:0,
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  init_args(exact_only, f_sys_show_ascii_art))).
*/
/*
(show-ascii-art)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:853 **********************/
:-lisp_compile_to_prolog(pkg_sys,['show-ascii-art'])
:- f_sys_show_ascii_art(_Ignored).
/*
(defmacro eval-when-tl ((&rest when) &body body) (if (or (member 'eval when) (member ':execute when)) `(progn ,@body) nil)) 


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:872 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'eval-when-tl',[['&rest',when],'&body',body],[if,[or,[member,[quote,eval],when],[member,[quote,':execute'],when]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',body]]],[]]])
wl:lambda_def(defmacro, sys_eval_when_tl, f_sys_eval_when_tl, [[c38_rest, when], c38_body, sys_body], [progn, [if, [or, [member, [quote, eval], when], [member, [quote, kw_execute], when]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_body]]], []]]).
wl:arglist_info(sys_eval_when_tl, f_sys_eval_when_tl, [[c38_rest, when], c38_body, sys_body], arginfo{all:0, allow_other_keys:0, aux:0, body:[sys_body], complex:[body], env:0, key:0, names:[sys_body, when], opt:0, req:0, rest:[sys_body], sublists:0, whole:0}).
wl: init_args(1, f_sys_eval_when_tl).

/*

### Compiled:  `SYS::EVAL-WHEN-TL` 
*/
f_sys_eval_when_tl(When_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_body, Body_In), bv(when, When_In)|SubEnv],
	global_env(SubEnv),
	as_body(sys_body, Body_In, RestNKeys),
	(   get_var(Env, when, When_Get),
	    cl_member(eval, When_Get, [], FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(Env, when, When_Get13),
	    cl_member(kw_execute, When_Get13, [], Member_Ret),
	    IFTEST=Member_Ret
	),
	(   IFTEST\==[]
	->  get_var(Env, sys_body, Body_Get),
	    _4490634=[progn|Body_Get]
	;   _4490634=[]
	),
	_4490634=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_eval_when_tl, classof, claz_macro),
   set_opv(sys_eval_when_tl, compile_as, kw_operator),
   set_opv(sys_eval_when_tl, function, f_sys_eval_when_tl),
   DefMacroResult=sys_eval_when_tl.
/*
:- side_effect(assert_lsp(sys_eval_when_tl,
			  lambda_def(defmacro,
				     sys_eval_when_tl,
				     f_sys_eval_when_tl,
				     [[c38_rest, when], c38_body, sys_body],
				     
				     [ progn,
				       
				       [ if,
					 
					 [ or,
					   [member, [quote, eval], when],
					   [member, [quote, kw_execute], when]
					 ],
					 
					 [ '#BQ',
					   
					   [ progn,
					     ['#BQ-COMMA-ELIPSE', sys_body]
					   ]
					 ],
					 []
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_eval_when_tl,
			  arglist_info(sys_eval_when_tl,
				       f_sys_eval_when_tl,
				       [[c38_rest, when], c38_body, sys_body],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:[sys_body],
						complex:[body],
						env:0,
						key:0,
						names:[sys_body, when],
						opt:0,
						req:0,
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_eval_when_tl, init_args(1, f_sys_eval_when_tl))).
*/
/*
(in-package "SYSTEM")


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:999 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored).
/*
(defmacro setf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (cond
                ((zerop nargs) nil)
                ((= nargs 2)
                 (let ((place (car pairs))
                       (value-form (cadr pairs)))
                   (cond
                    ((symbolp place)
                     `(setq ,place ,value-form))
                    ((consp place)
                     (if (eq (car place) 'the)
                         `(setf ,(caddr place) (the ,(cadr place) ,value-form))
                       (multiple-value-bind (temps vars newvals setter getter)
                           (get-setf-expansion place env)
                         (declare (ignore getter))
                         `(let (,@(mapcar #'list temps vars))
                            (multiple-value-bind ,newvals ,value-form
                              ,setter))))))))
                (t
                 (do* ((pairs pairs (cddr pairs))
                       (setfs (list 'progn))
                       (splice setfs))
                      ((endp pairs) setfs)
                   (setq splice (cdr (rplacd splice
                                             `((setf ,(car pairs) ,(cadr pairs)))))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1023 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,setf,['&rest',pairs,'&environment',env],[let,[[nargs,[length,pairs]]],[assert,[evenp,nargs]],[cond,[[zerop,nargs],[]],[[=,nargs,2],[let,[[place,[car,pairs]],['value-form',[cadr,pairs]]],[cond,[[symbolp,place],['#BQ',[setq,['#COMMA',place],['#COMMA','value-form']]]],[[consp,place],[if,[eq,[car,place],[quote,the]],['#BQ',[setf,['#COMMA',[caddr,place]],[the,['#COMMA',[cadr,place]],['#COMMA','value-form']]]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],[declare,[ignore,getter]],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],['#COMMA','value-form'],['#COMMA',setter]]]]]]]]]],[t,['do*',[[pairs,pairs,[cddr,pairs]],[setfs,[list,[quote,progn]]],[splice,setfs]],[[endp,pairs],setfs],[setq,splice,[cdr,[rplacd,splice,['#BQ',[[setf,['#COMMA',[car,pairs]],['#COMMA',[cadr,pairs]]]]]]]]]]]]])
wl:lambda_def(defmacro, setf, cl_setf, [c38_rest, sys_pairs, c38_environment, env], [progn, [let, [[sys_nargs, [length, sys_pairs]]], [assert, [evenp, sys_nargs]], [cond, [[zerop, sys_nargs], []], [[=, sys_nargs, 2], [let, [[sys_place, [car, sys_pairs]], [sys_value_form, [cadr, sys_pairs]]], [cond, [[symbolp, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]]], [[consp, sys_place], [if, [eq, [car, sys_place], [quote, the]], ['#BQ', [setf, ['#COMMA', [caddr, sys_place]], [the, ['#COMMA', [cadr, sys_place]], ['#COMMA', sys_value_form]]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [declare, [ignore, sys_getter]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]]]]]]]], [t, [do_xx, [[sys_pairs, sys_pairs, [cddr, sys_pairs]], [sys_setfs, [list, [quote, progn]]], [sys_splice, sys_setfs]], [[endp, sys_pairs], sys_setfs], [setq, sys_splice, [cdr, [rplacd, sys_splice, ['#BQ', [[setf, ['#COMMA', [car, sys_pairs]], ['#COMMA', [cadr, sys_pairs]]]]]]]]]]]]]).
wl: declared(cl_setf, env_arg1).

wl:arglist_info(setf, cl_setf, [c38_rest, sys_pairs, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_pairs, env], opt:0, req:0, rest:[sys_pairs], sublists:0, whole:0}).
wl: init_args(0, cl_setf).

/*

### Compiled:  `CL:SETF` 
*/
cl_setf(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_pairs, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	catch(( ( get_var(Env, sys_pairs, Pairs_Get),
		  cl_length(Pairs_Get, Nargs_Init),
		  LEnv=[bv(sys_nargs, Nargs_Init)|Env],
		  cl_assert([evenp, sys_nargs], Assert_Ret),
		  get_var(LEnv, sys_nargs, Nargs_Get),
		  (   mth:is_zerop(Nargs_Get)
		  ->  LetResult23=[]
		  ;   get_var(LEnv, sys_nargs, Nargs_Get19),
		      (   Nargs_Get19=:=2
		      ->  get_var(LEnv, sys_pairs, Pairs_Get25),
			  cl_car(Pairs_Get25, Place_Init),
			  get_var(LEnv, sys_pairs, Pairs_Get26),
			  cl_cadr(Pairs_Get26, Value_form_Init),
			  LEnv24=[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)|LEnv],
			  get_var(LEnv24, sys_place, Place_Get),
			  (   is_symbolp(Place_Get)
			  ->  get_var(LEnv24, sys_place, Place_Get33),
			      get_var(LEnv24, sys_value_form, Value_form_Get),
			      LetResult23=[setq, Place_Get33, Value_form_Get]
			  ;   get_var(LEnv24, sys_place, Place_Get36),
			      (   is_consp(Place_Get36)
			      ->  get_var(LEnv24, sys_place, Place_Get40),
				  cl_car(Place_Get40, PredArg1Result42),
				  (   is_eq(PredArg1Result42, the)
				  ->  get_var(LEnv24, sys_place, Place_Get43),
				      cl_caddr(Place_Get43, Caddr_Ret),
				      get_var(LEnv24, sys_place, Place_Get44),
				      cl_cadr(Place_Get44, Cadr_Ret),
				      get_var(LEnv24,
					      sys_value_form,
					      Value_form_Get45),
				      LetResult23=[setf, Caddr_Ret, [the, Cadr_Ret, Value_form_Get45]]
				  ;   LEnv48=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv24],
				      get_var(LEnv48, env, Env_Get),
				      get_var(LEnv48, sys_place, Place_Get49),
				      cl_get_setf_expansion(Place_Get49,
							    Env_Get,
							    Setf_expansion_Ret),
				      setq_from_values(LEnv48,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ]),
				      cl_declare([ignore, sys_getter],
						 Declare_Ret),
				      get_var(LEnv48, sys_temps, Temps_Get),
				      get_var(LEnv48, sys_vars, Vars_Get),
				      cl_mapcar(cl_list,
						[Temps_Get, Vars_Get],
						Mapcar_Ret),
				      get_var(LEnv48, sys_newvals, Newvals_Get),
				      get_var(LEnv48, sys_setter, Setter_Get),
				      get_var(LEnv48,
					      sys_value_form,
					      Value_form_Get54),
				      LetResult23=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Value_form_Get54, Setter_Get]]
				  )
			      ;   ElseResult=[],
				  LetResult23=ElseResult
			      )
			  )
		      ;   get_var(LEnv, sys_pairs, Pairs_Get62),
			  Setfs_Init=progn,
			  get_var(LEnv, sys_setfs, Setfs_Get),
			  AEnv=[bv(sys_pairs, Pairs_Get62), bv(sys_setfs, Setfs_Init), bv(sys_splice, Setfs_Get)|LEnv],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_1), get_var(AEnv, sys_pairs, Pairs_Get87), (is_endp(Pairs_Get87)->get_var(AEnv, sys_setfs, RetResult90), throw(block_exit([], RetResult90)), _TBResult=ThrowResult91;get_var(AEnv, sys_pairs, Pairs_Get96), get_var(AEnv, sys_splice, Splice_Get95), cl_car(Pairs_Get96, Car_Ret), get_var(AEnv, sys_pairs, Pairs_Get97), cl_cadr(Pairs_Get97, Cadr_Ret121), cl_rplacd(Splice_Get95, [[setf, Car_Ret, Cadr_Ret121]], Cdr_Param), cl_cdr(Cdr_Param, Splice), set_var(AEnv, sys_splice, Splice), get_var(AEnv, sys_pairs, Pairs_Get98), cl_cddr(Pairs_Get98, Pairs), set_var(AEnv, sys_pairs, Pairs), goto(do_label_1, AEnv), _TBResult=_GORES99)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_pairs, Pairs_Get69), (is_endp(Pairs_Get69)->get_var(AEnv, sys_setfs, Setfs_Get74), throw(block_exit([], Setfs_Get74)), _7039978=ThrowResult;get_var(AEnv, sys_pairs, Pairs_Get78), get_var(AEnv, sys_splice, Rplacd_Param), cl_car(Pairs_Get78, Car_Ret122), get_var(AEnv, sys_pairs, Pairs_Get79), cl_cadr(Pairs_Get79, Cadr_Ret123), cl_rplacd(Rplacd_Param, [[setf, Car_Ret122, Cadr_Ret123]], Cdr_Param112), cl_cdr(Cdr_Param112, Cdr_Ret), set_var(AEnv, sys_splice, Cdr_Ret), get_var(AEnv, sys_pairs, Pairs_Get80), cl_cddr(Pairs_Get80, Cddr_Ret), set_var(AEnv, sys_pairs, Cddr_Ret), goto(do_label_1, AEnv), _7039978=_GORES)))
						  ]),
				  []=LetResult60
				),
				block_exit([], LetResult60),
				true),
			  LetResult23=LetResult60
		      )
		  )
		),
		LetResult23=MFResult
	      ),
	      block_exit(setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_setf, classof, claz_macro),
   set_opv(setf, compile_as, kw_operator),
   set_opv(setf, function, cl_setf),
   DefMacroResult=setf.
/*
:- side_effect(assert_lsp(setf,
			  lambda_def(defmacro,
				     setf,
				     cl_setf,
				     [c38_rest, sys_pairs, c38_environment, env],
				     
				     [ progn,
				       
				       [ let,
					 [[sys_nargs, [length, sys_pairs]]],
					 [assert, [evenp, sys_nargs]],
					 
					 [ cond,
					   [[zerop, sys_nargs], []],
					   
					   [ [=, sys_nargs, 2],
					     
					     [ let,
					       
					       [ [sys_place, [car, sys_pairs]],
						 
						 [ sys_value_form,
						   [cadr, sys_pairs]
						 ]
					       ],
					       
					       [ cond,
						 
						 [ [symbolp, sys_place],
						   
						   [ '#BQ',
						     
						     [ setq,
						       ['#COMMA', sys_place],
						       
						       [ '#COMMA',
							 sys_value_form
						       ]
						     ]
						   ]
						 ],
						 
						 [ [consp, sys_place],
						   
						   [ if,
						     
						     [ eq,
						       [car, sys_place],
						       [quote, the]
						     ],
						     
						     [ '#BQ',
						       
						       [ setf,
							 
							 [ '#COMMA',
							   [caddr, sys_place]
							 ],
							 
							 [ the,
							   
							   [ '#COMMA',
							     [cadr, sys_place]
							   ],
							   
							   [ '#COMMA',
							     sys_value_form
							   ]
							 ]
						       ]
						     ],
						     
						     [ multiple_value_bind,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ],
						       
						       [ get_setf_expansion,
							 sys_place,
							 env
						       ],
						       
						       [ declare,
							 [ignore, sys_getter]
						       ],
						       
						       [ '#BQ',
							 
							 [ let,
							   
							   [ 
							     [ '#BQ-COMMA-ELIPSE',
							       
							       [ mapcar,
								 function(list),
								 sys_temps,
								 sys_vars
							       ]
							     ]
							   ],
							   
							   [ multiple_value_bind,
							     
							     [ '#COMMA',
							       sys_newvals
							     ],
							     
							     [ '#COMMA',
							       sys_value_form
							     ],
							     
							     [ '#COMMA',
							       sys_setter
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
					   
					   [ t,
					     
					     [ do_xx,
					       
					       [ 
						 [ sys_pairs,
						   sys_pairs,
						   [cddr, sys_pairs]
						 ],
						 
						 [ sys_setfs,
						   [list, [quote, progn]]
						 ],
						 [sys_splice, sys_setfs]
					       ],
					       [[endp, sys_pairs], sys_setfs],
					       
					       [ setq,
						 sys_splice,
						 
						 [ cdr,
						   
						   [ rplacd,
						     sys_splice,
						     
						     [ '#BQ',
						       
						       [ 
							 [ setf,
							   
							   [ '#COMMA',
							     [car, sys_pairs]
							   ],
							   
							   [ '#COMMA',
							     [cadr, sys_pairs]
							   ]
							 ]
						       ]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(setf,
			  arglist_info(setf,
				       cl_setf,
				       
				       [ c38_rest,
					 sys_pairs,
					 c38_environment,
					 env
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[env],
						key:0,
						names:[sys_pairs, env],
						opt:0,
						req:0,
						rest:[sys_pairs],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf, init_args(0, cl_setf))).
*/
/*
(defmacro psetf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (if (< nargs 4)
                   `(progn (setf ,@pairs) nil)
                 (let ((setters nil))
                   (labels ((expand (pairs)
                                    (if pairs
                                        (multiple-value-bind (temps vars newvals setter getter)
                                            (get-setf-expansion (car pairs) env)
                                          (declare (ignore getter))
                                          (setq setters (cons setter setters))
                                          `(let (,@(mapcar #'list temps vars))
                                             (multiple-value-bind ,newvals ,(cadr pairs)
                                               ,(expand (cddr pairs)))))
                                      `(progn ,@setters nil))))
                     (expand pairs))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:2307 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,psetf,['&rest',pairs,'&environment',env],[let,[[nargs,[length,pairs]]],[assert,[evenp,nargs]],[if,[<,nargs,4],['#BQ',[progn,[setf,['#BQ-COMMA-ELIPSE',pairs]],[]]],[let,[[setters,[]]],[labels,[[expand,[pairs],[if,pairs,['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',[car,pairs],env],[declare,[ignore,getter]],[setq,setters,[cons,setter,setters]],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],['#COMMA',[cadr,pairs]],['#COMMA',[expand,[cddr,pairs]]]]]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',setters],[]]]]]],[expand,pairs]]]]]])
wl:lambda_def(defmacro, psetf, cl_psetf, [c38_rest, sys_pairs, c38_environment, env], [progn, [let, [[sys_nargs, [length, sys_pairs]]], [assert, [evenp, sys_nargs]], [if, [<, sys_nargs, 4], ['#BQ', [progn, [setf, ['#BQ-COMMA-ELIPSE', sys_pairs]], []]], [let, [[sys_setters, []]], [labels, [[sys_expand, [sys_pairs], [if, sys_pairs, [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_pairs], env], [declare, [ignore, sys_getter]], [setq, sys_setters, [cons, sys_setter, sys_setters]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [cadr, sys_pairs]], ['#COMMA', [sys_expand, [cddr, sys_pairs]]]]]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_setters], []]]]]], [sys_expand, sys_pairs]]]]]]).
wl: declared(cl_psetf, env_arg1).

wl:arglist_info(psetf, cl_psetf, [c38_rest, sys_pairs, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_pairs, env], opt:0, req:0, rest:[sys_pairs], sublists:0, whole:0}).
wl: init_args(0, cl_psetf).

/*

### Compiled:  `CL:PSETF` 
*/
cl_psetf(RestNKeys, FnResult) :-
	nop(defmacro),
	Env47=[bv(sys_pairs, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env47, sys_pairs, Pairs_Get),
	cl_length(Pairs_Get, Nargs_Init),
	LEnv=[bv(sys_nargs, Nargs_Init)|Env47],
	cl_assert([evenp, sys_nargs], Assert_Ret),
	get_var(LEnv, sys_nargs, Nargs_Get),
	(   Nargs_Get<4
	->  get_var(LEnv, sys_pairs, Pairs_Get18),
	    LetResult=[progn, [setf|Pairs_Get18], []]
	;   LEnv21=[bv(sys_setters, [])|LEnv],
	    must_maplist(must_det_l,
			 
			 [ (assert_lsp(sys_expand, wl:lambda_def(defun, sys_expand, f_sys_expand1, [sys_pairs], [[if, sys_pairs, [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_pairs], env], [declare, [ignore, sys_getter]], [setq, sys_setters, [cons, sys_setter, sys_setters]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [cadr, sys_pairs]], ['#COMMA', [sys_expand, [cddr, sys_pairs]]]]]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_setters], []]]]])), assert_lsp(sys_expand, wl:arglist_info(sys_expand, f_sys_expand1, [sys_pairs], arginfo{all:[sys_pairs], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_pairs], opt:0, req:[sys_pairs], rest:0, sublists:0, whole:0})), !, assert_lsp(sys_expand, wl:init_args(exact_only, f_sys_expand1)), assert_lsp(sys_expand,  (f_sys_expand1(Pairs_In24, FnResult23):-Env48=[bv(sys_pairs, Pairs_In24)|LEnv21], global_env(LEnv21), get_var(Env48, sys_pairs, IFTEST25), (IFTEST25\==[]->LEnv30=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|Env48], get_var(LEnv30, sys_pairs, Pairs_Get31), cl_car(Pairs_Get31, Setf_expansion_Param), get_var(LEnv30, env, Env_Get), cl_get_setf_expansion(Setf_expansion_Param, Env_Get, Setf_expansion_Ret), setq_from_values(LEnv30, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter]), cl_declare([ignore, sys_getter], Declare_Ret), get_var(LEnv30, sys_setter, Setter_Get), get_var(LEnv30, sys_setters, Setters_Get), Setters=[Setter_Get|Setters_Get], set_var(LEnv30, sys_setters, Setters), get_var(LEnv30, sys_temps, Temps_Get), get_var(LEnv30, sys_vars, Vars_Get), cl_mapcar(cl_list, [Temps_Get, Vars_Get], Mapcar_Ret), get_var(LEnv30, sys_newvals, Newvals_Get), get_var(LEnv30, sys_pairs, Pairs_Get39), cl_cadr(Pairs_Get39, Cadr_Ret), get_var(LEnv30, sys_pairs, Pairs_Get40), cl_cddr(Pairs_Get40, Expand_Param), f_sys_expand(Expand_Param, Expand_Ret), FnResult23=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Cadr_Ret, Expand_Ret]];get_var(Env48, sys_setters, Setters_Get41), bq_append([progn|Setters_Get41], [[]], ElseResult), FnResult23=ElseResult))))
			 ]),
	    get_var(LEnv21, sys_pairs, Pairs_Get44),
	    f_sys_expand1(Pairs_Get44, LetResult20),
	    LetResult=LetResult20
	),
	LetResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_psetf, classof, claz_macro),
   set_opv(psetf, compile_as, kw_operator),
   set_opv(psetf, function, cl_psetf),
   DefMacroResult=psetf.

/*
(defmacro shiftf (&rest places-and-newvalue &environment env)
             (let ((nargs (length places-and-newvalue)))
               (assert (>= nargs 2))
               (let ((place (car places-and-newvalue)))
                 (multiple-value-bind (temps vars newvals setter getter)
                     (get-setf-expansion place env)
                   `(let (,@(mapcar #'list temps vars))
                      (multiple-value-prog1 ,getter
                        (multiple-value-bind ,newvals
                            ,(if (= nargs 2)
                                 (cadr places-and-newvalue)
                               `(shiftf ,@(cdr places-and-newvalue)))
                          ,setter)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:3312 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,shiftf,['&rest','places-and-newvalue','&environment',env],[let,[[nargs,[length,'places-and-newvalue']]],[assert,[>=,nargs,2]],[let,[[place,[car,'places-and-newvalue']]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-prog1',['#COMMA',getter],['multiple-value-bind',['#COMMA',newvals],['#COMMA',[if,[=,nargs,2],[cadr,'places-and-newvalue'],['#BQ',[shiftf,['#BQ-COMMA-ELIPSE',[cdr,'places-and-newvalue']]]]]],['#COMMA',setter]]]]]]]]])
wl:lambda_def(defmacro, shiftf, cl_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, env], [progn, [let, [[sys_nargs, [length, sys_places_and_newvalue]]], [assert, [>=, sys_nargs, 2]], [let, [[sys_place, [car, sys_places_and_newvalue]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_prog1, ['#COMMA', sys_getter], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [if, [=, sys_nargs, 2], [cadr, sys_places_and_newvalue], ['#BQ', [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places_and_newvalue]]]]]], ['#COMMA', sys_setter]]]]]]]]]).
wl: declared(cl_shiftf, env_arg1).

wl:arglist_info(shiftf, cl_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_places_and_newvalue, env], opt:0, req:0, rest:[sys_places_and_newvalue], sublists:0, whole:0}).
wl: init_args(0, cl_shiftf).

/*

### Compiled:  `CL:SHIFTF` 
*/
cl_shiftf(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_places_and_newvalue, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_places_and_newvalue, Places_and_newvalue_Get),
	cl_length(Places_and_newvalue_Get, Nargs_Init),
	LEnv=[bv(sys_nargs, Nargs_Init)|Env],
	cl_assert([>=, sys_nargs, 2], Assert_Ret),
	get_var(LEnv, sys_places_and_newvalue, Places_and_newvalue_Get17),
	cl_car(Places_and_newvalue_Get17, Place_Init),
	LEnv16=[bv(sys_place, Place_Init)|LEnv],
	LEnv21=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv16],
	get_var(LEnv21, env, Env_Get),
	get_var(LEnv21, sys_place, Place_Get),
	cl_get_setf_expansion(Place_Get, Env_Get, Setf_expansion_Ret),
	setq_from_values(LEnv21,
			 
			 [ sys_temps,
			   sys_vars,
			   sys_newvals,
			   sys_setter,
			   sys_getter
			 ]),
	get_var(LEnv21, sys_temps, Temps_Get),
	get_var(LEnv21, sys_vars, Vars_Get),
	cl_mapcar(cl_list, [Temps_Get, Vars_Get], Mapcar_Ret),
	get_var(LEnv21, sys_getter, Getter_Get),
	get_var(LEnv21, sys_nargs, Nargs_Get),
	get_var(LEnv21, sys_newvals, Newvals_Get),
	(   Nargs_Get=:=2
	->  get_var(LEnv21, sys_places_and_newvalue, Places_and_newvalue_Get32),
	    cl_cadr(Places_and_newvalue_Get32, TrueResult),
	    CAR=TrueResult
	;   get_var(LEnv21, sys_places_and_newvalue, Places_and_newvalue_Get33),
	    cl_cdr(Places_and_newvalue_Get33, Cdr_Ret),
	    CAR=[shiftf|Cdr_Ret]
	),
	get_var(LEnv21, sys_setter, Setter_Get),
	[let, Mapcar_Ret, [multiple_value_prog1, Getter_Get, [multiple_value_bind, Newvals_Get, CAR, Setter_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_shiftf, classof, claz_macro),
   set_opv(shiftf, compile_as, kw_operator),
   set_opv(shiftf, function, cl_shiftf),
   DefMacroResult=shiftf.

/*
(defmacro rotatef (&rest places &environment env)
             (if (< (length places) 2)
                 nil
               (multiple-value-bind (temps vars newvals setter getter)
                   (get-setf-expansion (car places) env)
                 `(let (,@(mapcar #'list temps vars))
                    (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
                      ,setter)
                    nil))))


;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4028 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,rotatef,['&rest',places,'&environment',env],[if,[<,[length,places],2],[],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',[car,places],env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],[shiftf,['#BQ-COMMA-ELIPSE',[cdr,places]],['#COMMA',getter]],['#COMMA',setter]],[]]]]]])
wl:lambda_def(defmacro, rotatef, cl_rotatef, [c38_rest, sys_places, c38_environment, env], [progn, [if, [<, [length, sys_places], 2], [], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_places], env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places]], ['#COMMA', sys_getter]], ['#COMMA', sys_setter]], []]]]]]).
wl: declared(cl_rotatef, env_arg1).

wl:arglist_info(rotatef, cl_rotatef, [c38_rest, sys_places, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_places, env], opt:0, req:0, rest:[sys_places], sublists:0, whole:0}).
wl: init_args(0, cl_rotatef).

/*

### Compiled:  `CL:ROTATEF` 
*/
cl_rotatef(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_places, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_places, Places_Get),
	cl_length(Places_Get, PredArg1Result),
	(   PredArg1Result<2
	->  _20912602=[]
	;   LEnv=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, sys_places, Places_Get16),
	    cl_car(Places_Get16, Setf_expansion_Param),
	    get_var(LEnv, env, Env_Get),
	    cl_get_setf_expansion(Setf_expansion_Param,
				  Env_Get,
				  Setf_expansion_Ret),
	    setq_from_values(LEnv,
			     
			     [ sys_temps,
			       sys_vars,
			       sys_newvals,
			       sys_setter,
			       sys_getter
			     ]),
	    get_var(LEnv, sys_temps, Temps_Get),
	    get_var(LEnv, sys_vars, Vars_Get),
	    cl_mapcar(cl_list, [Temps_Get, Vars_Get], Mapcar_Ret),
	    get_var(LEnv, sys_newvals, Newvals_Get),
	    get_var(LEnv, sys_places, Places_Get21),
	    cl_cdr(Places_Get21, Cdr_Ret),
	    get_var(LEnv, sys_getter, Getter_Get),
	    bq_append([shiftf|Cdr_Ret], [Getter_Get], Bq_append_Ret),
	    get_var(LEnv, sys_setter, Setter_Get),
	    _20912602=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Bq_append_Ret, Setter_Get], []]
	),
	_20912602=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_rotatef, classof, claz_macro),
   set_opv(rotatef, compile_as, kw_operator),
   set_opv(rotatef, function, cl_rotatef),
   DefMacroResult=rotatef.
/*
; Adapted from SBCL.
*/
/*
(defmacro push (&environment env item place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (cons ,g ,getter)))
             ,setter)))))

;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4485 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,push,['&environment',env,item,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[cons,['#COMMA',item],['#COMMA',place]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[cons,['#COMMA',g],['#COMMA',getter]]]],['#COMMA',setter]]]]]]])
wl:lambda_def(defmacro, push, cl_push, [c38_environment, env, item, sys_place], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [setq, ['#COMMA', sys_place], [cons, ['#COMMA', item], ['#COMMA', sys_place]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [cons, ['#COMMA', sys_g], ['#COMMA', sys_getter]]]], ['#COMMA', sys_setter]]]]]]]).
wl: declared(cl_push, env_arg1).

wl:arglist_info(push, cl_push, [c38_environment, env, item, sys_place], arginfo{all:[item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[env], key:0, names:[env, item, sys_place], opt:0, req:[item, sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_push).

/*

### Compiled:  `CL:PUSH` 
*/
cl_push(Item_In, Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(item, Item_In), bv(sys_place, Place_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_place, Place_Get),
	(   is_symbolp(Place_Get)
	->  get_var(Env, env, Env_Get),
	    get_var(Env, sys_place, Place_Get16),
	    cl_macroexpand([Place_Get16, Env_Get], Macroexpand_Ret),
	    cl_eq(Place_Get16, Macroexpand_Ret, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env, item, Item_Get),
	    get_var(Env, sys_place, Place_Get20),
	    _22831648=[setq, Place_Get20, [cons, Item_Get, Place_Get20]]
	;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, env, Env_Get27),
	    get_var(LEnv, sys_place, Place_Get26),
	    cl_get_setf_expansion(Place_Get26, Env_Get27, Setf_expansion_Ret),
	    setq_from_values(LEnv,
			     
			     [ sys_dummies,
			       sys_vals,
			       sys_newval,
			       sys_setter,
			       sys_getter
			     ]),
	    cl_gensym(G_Init),
	    LEnv30=[bv(sys_g, G_Init)|LEnv],
	    get_var(LEnv30, item, Item_Get33),
	    get_var(LEnv30, sys_dummies, Dummies_Get),
	    get_var(LEnv30, sys_g, G_Get),
	    get_var(LEnv30, sys_vals, Vals_Get),
	    cl_mapcar(cl_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
	    get_var(LEnv30, sys_newval, Newval_Get),
	    cl_car(Newval_Get, Car_Ret),
	    get_var(LEnv30, sys_g, G_Get37),
	    get_var(LEnv30, sys_getter, Getter_Get),
	    bq_append([[G_Get, Item_Get33]|Mapcar_Ret],
		      [[Car_Ret, [cons, G_Get37, Getter_Get]]],
		      Bq_append_Ret),
	    get_var(LEnv30, sys_setter, Setter_Get),
	    _22831648=[let_xx, Bq_append_Ret, Setter_Get]
	),
	_22831648=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_push, classof, claz_macro),
   set_opv(push, compile_as, kw_operator),
   set_opv(push, function, cl_push),
   DefMacroResult=push.

/*
; Adapted from SBCL.
*/
/*
(defmacro pushnew (&environment env item place &rest keys)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (adjoin ,g ,getter ,@keys)))
             ,setter)))))

;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4948 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pushnew,['&environment',env,item,place,'&rest',keys],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[adjoin,['#COMMA',item],['#COMMA',place],['#BQ-COMMA-ELIPSE',keys]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[adjoin,['#COMMA',g],['#COMMA',getter],['#BQ-COMMA-ELIPSE',keys]]]],['#COMMA',setter]]]]]]])
wl:lambda_def(defmacro, pushnew, cl_pushnew, [c38_environment, env, item, sys_place, c38_rest, sys_keys], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [setq, ['#COMMA', sys_place], [adjoin, ['#COMMA', item], ['#COMMA', sys_place], ['#BQ-COMMA-ELIPSE', sys_keys]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [adjoin, ['#COMMA', sys_g], ['#COMMA', sys_getter], ['#BQ-COMMA-ELIPSE', sys_keys]]]], ['#COMMA', sys_setter]]]]]]]).
wl: declared(cl_pushnew, env_arg1).

wl:arglist_info(pushnew, cl_pushnew, [c38_environment, env, item, sys_place, c38_rest, sys_keys], arginfo{all:[item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment, rest], env:[env], key:0, names:[env, item, sys_place, sys_keys], opt:0, req:[item, sys_place], rest:[sys_keys], sublists:0, whole:0}).
wl: init_args(2, cl_pushnew).

/*

### Compiled:  `CL:PUSHNEW` 
*/
cl_pushnew(Item_In, Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(item, Item_In), bv(sys_place, Place_In), bv(sys_keys, RestNKeys)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_place, Place_Get),
	(   is_symbolp(Place_Get)
	->  get_var(Env, env, Env_Get),
	    get_var(Env, sys_place, Place_Get17),
	    cl_macroexpand([Place_Get17, Env_Get], Macroexpand_Ret),
	    cl_eq(Place_Get17, Macroexpand_Ret, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env, item, Item_Get),
	    get_var(Env, sys_keys, Keys_Get),
	    get_var(Env, sys_place, Place_Get21),
	    _25518664=[setq, Place_Get21, [adjoin, Item_Get, Place_Get21|Keys_Get]]
	;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, env, Env_Get29),
	    get_var(LEnv, sys_place, Place_Get28),
	    cl_get_setf_expansion(Place_Get28, Env_Get29, Setf_expansion_Ret),
	    setq_from_values(LEnv,
			     
			     [ sys_dummies,
			       sys_vals,
			       sys_newval,
			       sys_setter,
			       sys_getter
			     ]),
	    cl_gensym(G_Init),
	    LEnv32=[bv(sys_g, G_Init)|LEnv],
	    get_var(LEnv32, item, Item_Get35),
	    get_var(LEnv32, sys_dummies, Dummies_Get),
	    get_var(LEnv32, sys_g, G_Get),
	    get_var(LEnv32, sys_vals, Vals_Get),
	    cl_mapcar(cl_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
	    get_var(LEnv32, sys_newval, Newval_Get),
	    cl_car(Newval_Get, Car_Ret),
	    get_var(LEnv32, sys_g, G_Get39),
	    get_var(LEnv32, sys_getter, Getter_Get),
	    get_var(LEnv32, sys_keys, Keys_Get41),
	    bq_append([[G_Get, Item_Get35]|Mapcar_Ret],
		      [[Car_Ret, [adjoin, G_Get39, Getter_Get|Keys_Get41]]],
		      Bq_append_Ret),
	    get_var(LEnv32, sys_setter, Setter_Get),
	    _25518664=[let_xx, Bq_append_Ret, Setter_Get]
	),
	_25518664=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_pushnew, classof, claz_macro),
   set_opv(pushnew, compile_as, kw_operator),
   set_opv(pushnew, function, cl_pushnew),
   DefMacroResult=pushnew.
/*
; Adapted from SBCL.
*/
/*
(defmacro pop (&environment env place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (do* ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil))
             ((null d)
              (push (list (car newval) getter) let-list)
              `(let* ,(nreverse let-list)
                 (prog1 (car ,(car newval))
                        (setq ,(car newval) (cdr ,(car newval)))
                        ,setter)))
          (push (list (car d) (car v)) let-list)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:5443 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pop,['&environment',env,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[prog1,[car,['#COMMA',place]],[setq,['#COMMA',place],[cdr,['#COMMA',place]]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],['do*',[[d,dummies,[cdr,d]],[v,vals,[cdr,v]],['let-list',[]]],[[null,d],[push,[list,[car,newval],getter],'let-list'],['#BQ',['let*',['#COMMA',[nreverse,'let-list']],[prog1,[car,['#COMMA',[car,newval]]],[setq,['#COMMA',[car,newval]],[cdr,['#COMMA',[car,newval]]]],['#COMMA',setter]]]]],[push,[list,[car,d],[car,v]],'let-list']]]]])
wl:lambda_def(defmacro, pop, cl_pop, [c38_environment, env, sys_place], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [prog1, [car, ['#COMMA', sys_place]], [setq, ['#COMMA', sys_place], [cdr, ['#COMMA', sys_place]]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [do_xx, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, []]], [[null, sys_d], [push, [list, [car, sys_newval], sys_getter], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], [prog1, [car, ['#COMMA', [car, sys_newval]]], [setq, ['#COMMA', [car, sys_newval]], [cdr, ['#COMMA', [car, sys_newval]]]], ['#COMMA', sys_setter]]]]], [push, [list, [car, sys_d], [car, sys_v]], sys_let_list]]]]]).
wl: declared(cl_pop, env_arg1).

wl:arglist_info(pop, cl_pop, [c38_environment, env, sys_place], arginfo{all:[sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[env], key:0, names:[env, sys_place], opt:0, req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_pop).

/*

### Compiled:  `CL:POP` 
*/
cl_pop(Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(sys_place, Place_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	catch(( ( get_var(Env, sys_place, Place_Get),
		  (   is_symbolp(Place_Get)
		  ->  get_var(Env, env, Env_Get),
		      get_var(Env, sys_place, Place_Get15),
		      cl_macroexpand([Place_Get15, Env_Get], Macroexpand_Ret),
		      cl_eq(Place_Get15, Macroexpand_Ret, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(Env, sys_place, Place_Get19),
		      _28373318=[prog1, [car, Place_Get19], [setq, Place_Get19, [cdr, Place_Get19]]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
		      get_var(LEnv, env, Env_Get26),
		      get_var(LEnv, sys_place, Place_Get25),
		      cl_get_setf_expansion(Place_Get25,
					    Env_Get26,
					    Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      get_var(LEnv, sys_dummies, Dummies_Get),
		      get_var(LEnv, sys_vals, Vals_Get),
		      BlockExitEnv=[bv(sys_d, Dummies_Get), bv(sys_v, Vals_Get), bv(sys_let_list, [])|LEnv],
		      catch(( call_addr_block(BlockExitEnv,
					      (push_label(do_label_2), get_var(BlockExitEnv, sys_d, IFTEST53), (IFTEST53==[]->cl_push([list, [car, sys_newval], sys_getter], sys_let_list, [], Push_Ret), get_var(BlockExitEnv, sys_let_list, Let_list_Get58), cl_nreverse(Let_list_Get58, Nreverse_Ret), get_var(BlockExitEnv, sys_newval, Newval_Get59), cl_car(Newval_Get59, Car_Ret), get_var(BlockExitEnv, sys_newval, Newval_Get60), cl_car(Newval_Get60, Car_Ret84), get_var(BlockExitEnv, sys_newval, Newval_Get61), cl_car(Newval_Get61, Car_Ret85), get_var(BlockExitEnv, sys_setter, Setter_Get62), throw(block_exit([], [let_xx, Nreverse_Ret, [prog1, [car, Car_Ret], [setq, Car_Ret84, [cdr, Car_Ret85]], Setter_Get62]])), _TBResult=ThrowResult57;cl_push([list, [car, sys_d], [car, sys_v]], sys_let_list, [], Push_Ret86), get_var(BlockExitEnv, sys_d, D_Get64), cl_cdr(D_Get64, D), get_var(BlockExitEnv, sys_v, V_Get65), cl_cdr(V_Get65, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_2, BlockExitEnv), _TBResult=_GORES66)),
					      
					      [ addr(addr_tagbody_2_do_label_2,
						     do_label_2,
						     '$unused',
						     BlockExitEnv,
						     (get_var(BlockExitEnv, sys_d, IFTEST35), (IFTEST35==[]->cl_push([list, [car, sys_newval], sys_getter], sys_let_list, [], Push_Ret87), get_var(BlockExitEnv, sys_let_list, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret88), get_var(BlockExitEnv, sys_newval, Car_Param), cl_car(Car_Param, Car_Ret89), get_var(BlockExitEnv, sys_newval, Newval_Get42), cl_car(Newval_Get42, Car_Ret90), get_var(BlockExitEnv, sys_newval, Newval_Get43), cl_car(Newval_Get43, Car_Ret91), get_var(BlockExitEnv, sys_setter, Get_var_Ret), throw(block_exit([], [let_xx, Nreverse_Ret88, [prog1, [car, Car_Ret89], [setq, Car_Ret90, [cdr, Car_Ret91]], Get_var_Ret]])), _29138900=ThrowResult;cl_push([list, [car, sys_d], [car, sys_v]], sys_let_list, [], Push_Ret93), get_var(BlockExitEnv, sys_d, D_Get46), cl_cdr(D_Get46, Cdr_Ret), get_var(BlockExitEnv, sys_v, Cdr_Param), cl_cdr(Cdr_Param, Cdr_Ret95), set_var(BlockExitEnv, sys_d, Cdr_Ret), set_var(BlockExitEnv, sys_v, Cdr_Ret95), goto(do_label_2, BlockExitEnv), _29138900=_GORES)))
					      ]),
			      []=LetResult28
			    ),
			    block_exit([], LetResult28),
			    true),
		      _28373318=LetResult28
		  )
		),
		_28373318=MFResult
	      ),
	      block_exit(pop, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_pop, classof, claz_macro),
   set_opv(pop, compile_as, kw_operator),
   set_opv(pop, function, cl_pop),
   DefMacroResult=pop.  

/*
(defmacro make-hash-table (&rest all) `(make-instance 'hash-table ,@all))


#|
(defun make-hash-table (&key (test 'eql) (size 11) (rehash-size 1.5)
                             (rehash-threshold 0.75)
                             (weakness nil))
  (setf test (coerce-to-function test))
  (unless (and (integerp size) (>= size 0))
    (error 'type-error :datum size :expected-type '(integer 0)))
  (let ((size (max 11 (min size array-dimension-limit)))
        (weakness-types '(or (eql :key) (eql :value)
                             (eql :key-and-value)
                             (eql :key-or-value))))
    (if weakness
        (if (not (typep weakness weakness-types))
            (error 'type-error :datum weakness 
                   :expected-type weakness-types)
            (make-instance 'weak-hash-table test size rehash-size 
                                   rehash-threshold weakness))
	(make-instance 'hash-table test size 
                          rehash-size rehash-threshold))))
|#
    
  

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:6129 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'make-hash-table',['&rest',all],['#BQ',['make-instance',[quote,'hash-table'],['#BQ-COMMA-ELIPSE',all]]]])
wl:lambda_def(defmacro, make_hash_table, cl_make_hash_table, [c38_rest, sys_all], [progn, ['#BQ', [make_instance, [quote, hash_table], ['#BQ-COMMA-ELIPSE', sys_all]]]]).
wl:arglist_info(make_hash_table, cl_make_hash_table, [c38_rest, sys_all], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_all], opt:0, req:0, rest:[sys_all], sublists:0, whole:0}).
wl: init_args(0, cl_make_hash_table).

/*

### Compiled:  `CL:MAKE-HASH-TABLE` 
*/
cl_make_hash_table(Hash_table_Param, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_all, RestNKeys)|CDR],
	global_env(CDR),
	append([], RestNKeys, Hash_table_Param),
	get_var(Env, sys_all, All_Get),
	[make_instance, [quote, hash_table]|All_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_make_hash_table, classof, claz_macro),
   set_opv(make_hash_table, compile_as, kw_operator),
   set_opv(make_hash_table, function, cl_make_hash_table),
   DefMacroResult=make_hash_table.
/*
:- side_effect(assert_lsp(make_hash_table,
			  lambda_def(defmacro,
				     make_hash_table,
				     cl_make_hash_table,
				     [c38_rest, sys_all],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ make_instance,
					   [quote, hash_table],
					   ['#BQ-COMMA-ELIPSE', sys_all]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(make_hash_table,
			  arglist_info(make_hash_table,
				       cl_make_hash_table,
				       [c38_rest, sys_all],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_all],
						opt:0,
						req:0,
						rest:[sys_all],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(make_hash_table, init_args(0, cl_make_hash_table))).
*/
/*

(defun make-hash-table (&key (test 'eql) (size 11) (rehash-size 1.5)
                             (rehash-threshold 0.75)
                             (weakness nil))
  (setf test (coerce-to-function test))
  (unless (and (integerp size) (>= size 0))
    (error 'type-error :datum size :expected-type '(integer 0)))
  (let ((size (max 11 (min size array-dimension-limit)))
        (weakness-types '(or (eql :key) (eql :value)
                             (eql :key-and-value)
                             (eql :key-or-value))))
    (if weakness
        (if (not (typep weakness weakness-types))
            (error 'type-error :datum weakness 
                   :expected-type weakness-types)
            (make-instance 'weak-hash-table test size rehash-size 
                                   rehash-threshold weakness))
	(make-instance 'hash-table test size 
                          rehash-size rehash-threshold))))
*/
/*
(export 'compiler-macroexpand)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:7141 **********************/
:-lisp_compile_to_prolog(pkg_sys,[export,[quote,'compiler-macroexpand']])
:- cl_export(sys_compiler_macroexpand, _Ignored).
/*
(defvar *compiler-macros* (make-hash-table :test #'equal))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:7173 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*compiler-macros*',['make-hash-table',':test',function(equal)]])
:- cl_make_hash_table([kw_test, function(equal)], Xx_compiler_macros_xx),
   set_var(AEnv, sys_xx_compiler_macros_xx, Xx_compiler_macros_xx).
/*
(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (gethash1 name (the hash-table *compiler-macros*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:7233 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'compiler-macro-function',[name,'&optional',environment],[declare,[ignore,environment]],[gethash1,name,[the,'hash-table','*compiler-macros*']]])
wl:lambda_def(defun, compiler_macro_function, cl_compiler_macro_function, [sys_name, c38_optional, sys_environment], [[declare, [ignore, sys_environment]], [sys_gethash1, sys_name, [the, hash_table, sys_xx_compiler_macros_xx]]]).
wl:arglist_info(compiler_macro_function, cl_compiler_macro_function, [sys_name, c38_optional, sys_environment], arginfo{all:[sys_name, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_environment], opt:[sys_environment], req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_compiler_macro_function).

/*

### Compiled:  `CL:COMPILER-MACRO-FUNCTION` 
*/
cl_compiler_macro_function(Name_In, RestNKeys, FnResult) :-
	Env14=[bv(sys_name, Name_In), bv(sys_environment, Environment_In)|Env],
	global_env(Env),
	opt_var(Env, sys_environment, Environment_In, true, [], 1, RestNKeys),
	cl_declare([ignore, sys_environment], Declare_Ret),
	get_var(Env14, sys_name, Name_Get),
	get_var(Env14, sys_xx_compiler_macros_xx, Xx_compiler_macros_xx_Get),
	f_sys_gethash1(Name_Get, Xx_compiler_macros_xx_Get, Gethash1_Ret),
	Gethash1_Ret=FnResult.
:- set_opv(cl_compiler_macro_function, classof, claz_function),
   set_opv(compiler_macro_function, compile_as, kw_function),
   set_opv(compiler_macro_function, function, cl_compiler_macro_function),
   DefunResult=compiler_macro_function.
/*
:- side_effect(assert_lsp(compiler_macro_function,
			  lambda_def(defun,
				     compiler_macro_function,
				     cl_compiler_macro_function,
				     [sys_name, c38_optional, sys_environment],
				     
				     [ [declare, [ignore, sys_environment]],
				       
				       [ sys_gethash1,
					 sys_name,
					 
					 [ the,
					   hash_table,
					   sys_xx_compiler_macros_xx
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(compiler_macro_function,
			  arglist_info(compiler_macro_function,
				       cl_compiler_macro_function,
				       [sys_name, c38_optional, sys_environment],
				       arginfo{ all:[sys_name, sys_environment],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_environment
						      ],
						opt:[sys_environment],
						req:[sys_name],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(compiler_macro_function,
			  init_args(1, cl_compiler_macro_function))).
*/
/*
(defun (setf compiler-macro-function) (new-function name &optional environment)
  (declare (ignore environment))
  (setf (gethash name (the hash-table *compiler-macros*)) new-function))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:7381 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,[setf,'compiler-macro-function'],['new-function',name,'&optional',environment],[declare,[ignore,environment]],[setf,[gethash,name,[the,'hash-table','*compiler-macros*']],'new-function']])
/*
:- side_effect((compile_each($, _35070878, [[the, hash_table, sys_xx_compiler_macros_xx]], [[the, hash_table, sys_xx_compiler_macros_xx]], true), append([sys_name, [the, hash_table, sys_xx_compiler_macros_xx]], [CAR13, CAR], [sys_name, [the, hash_table, sys_xx_compiler_macros_xx], CAR13, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
/*
:- side_effect((compile_each($, _35070878, [[the, hash_table, sys_xx_compiler_macros_xx]], [[the, hash_table, sys_xx_compiler_macros_xx]], true), append([sys_name, [the, hash_table, sys_xx_compiler_macros_xx]], [CAR13, CAR], [sys_name, [the, hash_table, sys_xx_compiler_macros_xx], CAR13, CAR]), setf_inverse_op(gethash, []))).
*/
/*
:- side_effect((compile_each($, _35070878, [[the, hash_table, sys_xx_compiler_macros_xx]], [[the, hash_table, sys_xx_compiler_macros_xx]], true), append([sys_name, [the, hash_table, sys_xx_compiler_macros_xx]], [CAR13, CAR], [sys_name, [the, hash_table, sys_xx_compiler_macros_xx], CAR13, CAR]), setf_inverse_op(gethash, setf_gethash))).
*/
/*
:- side_effect((compile_each($, _35070878, [[the, hash_table, sys_xx_compiler_macros_xx]], [[the, hash_table, sys_xx_compiler_macros_xx]], true), append([sys_name, [the, hash_table, sys_xx_compiler_macros_xx]], [CAR13, CAR], [sys_name, [the, hash_table, sys_xx_compiler_macros_xx], CAR13, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
wl:lambda_def(defun, setf_compiler_macro_function, cl_setf_compiler_macro_function, ['&environment', '$env', sys_new_function, sys_name, c38_optional, sys_environment], [[declare, [ignore, sys_environment]], [setf, [gethash, sys_name, [the, hash_table, sys_xx_compiler_macros_xx]], sys_new_function]]).
wl: declared(cl_setf_compiler_macro_function, env_arg1).

wl:arglist_info(setf_compiler_macro_function, cl_setf_compiler_macro_function, ['&environment', '$env', sys_new_function, sys_name, c38_optional, sys_environment], arginfo{all:[sys_new_function, sys_name, sys_environment], allow_other_keys:0, aux:0, body:0, complex:[environment], env:['$env'], key:0, names:['$env', sys_new_function, sys_name, sys_environment], opt:[sys_environment], req:[sys_new_function, sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_setf_compiler_macro_function).

/*

### Compiled:  `CL::SETF-COMPILER-MACRO-FUNCTION` 
*/
cl_setf_compiler_macro_function(New_function_In, Name_In, RestNKeys, FnResult) :-
	Env19=[bv('$env', C36_env_In), bv(sys_new_function, New_function_In), bv(sys_name, Name_In), bv(sys_environment, Environment_In)|Env],
	global_env(Env),
	parent_env(C36_env_In),
	opt_var(Env, sys_environment, Environment_In, true, [], 1, RestNKeys),
	cl_declare([ignore, sys_environment], Declare_Ret),
	get_var(Env19, sys_name, Name_Get),
	get_var(Env19, sys_new_function, New_function_Get),
	get_var(Env19, sys_xx_compiler_macros_xx, Xx_compiler_macros_xx_Get),
	f_sys_puthash(Name_Get,
		      Xx_compiler_macros_xx_Get,
		      New_function_Get,
		      Puthash_Ret),
	Puthash_Ret=FnResult.
:- set_opv(cl_setf_compiler_macro_function, classof, claz_function),
   set_opv(setf_compiler_macro_function, compile_as, kw_function),
   set_opv(setf_compiler_macro_function,
	   function,
	   cl_setf_compiler_macro_function),
   DefunResult=setf_compiler_macro_function.
/*
:- side_effect(assert_lsp(setf_compiler_macro_function,
			  lambda_def(defun,
				     setf_compiler_macro_function,
				     cl_setf_compiler_macro_function,
				     
				     [ '&environment',
				       '$env',
				       sys_new_function,
				       sys_name,
				       c38_optional,
				       sys_environment
				     ],
				     
				     [ [declare, [ignore, sys_environment]],
				       
				       [ setf,
					 
					 [ gethash,
					   sys_name,
					   
					   [ the,
					     hash_table,
					     sys_xx_compiler_macros_xx
					   ]
					 ],
					 sys_new_function
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(setf_compiler_macro_function,
			  arglist_info(setf_compiler_macro_function,
				       cl_setf_compiler_macro_function,
				       
				       [ '&environment',
					 '$env',
					 sys_new_function,
					 sys_name,
					 c38_optional,
					 sys_environment
				       ],
				       arginfo{ all:
						    [ sys_new_function,
						      sys_name,
						      sys_environment
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment],
						env:['$env'],
						key:0,
						names:
						      [ '$env',
							sys_new_function,
							sys_name,
							sys_environment
						      ],
						opt:[sys_environment],
						req:
						    [ sys_new_function,
						      sys_name
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf_compiler_macro_function,
			  init_args(2, cl_setf_compiler_macro_function))).
*/
/*
(defmacro define-compiler-macro (name lambda-list &rest body)
  (let* ((form (gensym))
         (env (gensym))
         (block-name (fdefinition-block-name name)))
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list form body name 'defmacro :environment env
                        ;; when we encounter an error
                        ;; parsing the arguments in the call
                        ;; (not in the difinition!), return
                        ;; the arguments unmodified -- ie skip the
                        ;; transform (see also source-transform.lisp)
                        :error-fun `(lambda (&rest ignored)
                                      (declare (ignore ignored))
                                      (return-from ,block-name ,form)))
      (let ((expander `(lambda (,form ,env)
                         (declare (ignorable ,env))
                         (block ,block-name ,body))))
        `(progn
	   (record-source-information-for-type ',name :compiler-macro)
           (setf (compiler-macro-function ',name) (function ,expander))
           ',name)))))

;;; Adapted from OpenMCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:7568 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'define-compiler-macro',[name,'lambda-list','&rest',body],['let*',[[form,[gensym]],[env,[gensym]],['block-name',['fdefinition-block-name',name]]],['multiple-value-bind',[body,decls],['parse-defmacro','lambda-list',form,body,name,[quote,defmacro],':environment',env,':error-fun',['#BQ',[lambda,['&rest',ignored],[declare,[ignore,ignored]],['return-from',['#COMMA','block-name'],['#COMMA',form]]]]],[let,[[expander,['#BQ',[lambda,[['#COMMA',form],['#COMMA',env]],[declare,[ignorable,['#COMMA',env]]],[block,['#COMMA','block-name'],['#COMMA',body]]]]]],['#BQ',[progn,['record-source-information-for-type',[quote,['#COMMA',name]],':compiler-macro'],[setf,['compiler-macro-function',[quote,['#COMMA',name]]],[function,['#COMMA',expander]]],[quote,['#COMMA',name]]]]]]]])
wl:lambda_def(defmacro, define_compiler_macro, cl_define_compiler_type_macro, [sys_name, sys_lambda_list, c38_rest, sys_body], [progn, [let_xx, [[sys_form, [gensym]], [env, [gensym]], [sys_block_name, [sys_fdefinition_block_name, sys_name]]], [multiple_value_bind, [sys_body, sys_decls], [sys_parse_defmacro, sys_lambda_list, sys_form, sys_body, sys_name, [quote, defmacro], kw_environment, env, kw_error_fun, ['#BQ', [lambda, [c38_rest, sys_ignored], [declare, [ignore, sys_ignored]], [return_from, ['#COMMA', sys_block_name], ['#COMMA', sys_form]]]]], [let, [[sys_expander, ['#BQ', [lambda, [['#COMMA', sys_form], ['#COMMA', env]], [declare, [ignorable, ['#COMMA', env]]], [block, ['#COMMA', sys_block_name], ['#COMMA', sys_body]]]]]], ['#BQ', [progn, [sys_record_source_information_for_type, [quote, ['#COMMA', sys_name]], kw_compiler_macro], [setf, [compiler_macro_function, [quote, ['#COMMA', sys_name]]], [function, ['#COMMA', sys_expander]]], [quote, ['#COMMA', sys_name]]]]]]]]).
wl:arglist_info(define_compiler_macro, cl_define_compiler_type_macro, [sys_name, sys_lambda_list, c38_rest, sys_body], arginfo{all:[sys_name, sys_lambda_list], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_lambda_list, sys_body], opt:0, req:[sys_name, sys_lambda_list], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, cl_define_compiler_type_macro).

/*

### Compiled:  `CL:DEFINE-COMPILER-MACRO` 
*/
cl_define_compiler_type_macro(Name_In, Lambda_list_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_name, Name_In), bv(sys_lambda_list, Lambda_list_In), bv(sys_body, RestNKeys)|CDR],
	global_env(CDR),
	cl_gensym(Form_Init),
	cl_gensym(Env_Init),
	get_var(Env, sys_name, Name_Get),
	f_sys_fdefinition_block_name(Name_Get, Block_name_Init),
	LEnv=[bv(sys_form, Form_Init), bv(env, Env_Init), bv(sys_block_name, Block_name_Init)|Env],
	LEnv19=[bv(sys_body, []), bv(sys_decls, [])|LEnv],
	get_var(LEnv19, env, Env_Get),
	get_var(LEnv19, sys_block_name, Block_name_Get),
	get_var(LEnv19, sys_body, Body_Get),
	get_var(LEnv19, sys_form, Form_Get26),
	get_var(LEnv19, sys_lambda_list, Lambda_list_Get),
	get_var(LEnv19, sys_name, Name_Get23),
	f_sys_parse_deftype_macro(Lambda_list_Get,
				  Form_Get26,
				  Body_Get,
				  Name_Get23,
				  defmacro,
				  kw_environment,
				  Env_Get,
				  kw_error_fun,
				  
				  [ lambda,
				    [c38_rest, sys_ignored],
				    [declare, [ignore, sys_ignored]],
				    [return_from, Block_name_Get, Form_Get26]
				  ],
				  Deftype_macro_Ret),
	setq_from_values(LEnv19, [sys_body, sys_decls]),
	get_var(LEnv19, env, Env_Get31),
	get_var(LEnv19, sys_block_name, Block_name_Get33),
	get_var(LEnv19, sys_body, Body_Get34),
	get_var(LEnv19, sys_form, Form_Get30),
	LEnv29=[bv(sys_expander, [lambda, [Form_Get30, Env_Get31], [declare, [ignorable, Env_Get31]], [block, Block_name_Get33, Body_Get34]])|LEnv19],
	get_var(LEnv29, sys_expander, Expander_Get),
	get_var(LEnv29, sys_name, Name_Get35),
	[progn, [sys_record_source_information_for_type, [quote, Name_Get35], kw_compiler_macro], [setf, [compiler_macro_function, [quote, Name_Get35]], [function, Expander_Get]], [quote, Name_Get35]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_define_compiler_type_macro, classof, claz_macro),
   set_opv(define_compiler_macro, compile_as, kw_operator),
   set_opv(define_compiler_macro, function, cl_define_compiler_type_macro),
   DefMacroResult=define_compiler_macro.
/*
:- side_effect(assert_lsp(define_compiler_macro,
			  lambda_def(defmacro,
				     define_compiler_macro,
				     cl_define_compiler_type_macro,
				     
				     [ sys_name,
				       sys_lambda_list,
				       c38_rest,
				       sys_body
				     ],
				     
				     [ progn,
				       
				       [ let_xx,
					 
					 [ [sys_form, [gensym]],
					   [env, [gensym]],
					   
					   [ sys_block_name,
					     
					     [ sys_fdefinition_block_name,
					       sys_name
					     ]
					   ]
					 ],
					 
					 [ multiple_value_bind,
					   [sys_body, sys_decls],
					   
					   [ sys_parse_defmacro,
					     sys_lambda_list,
					     sys_form,
					     sys_body,
					     sys_name,
					     [quote, defmacro],
					     kw_environment,
					     env,
					     kw_error_fun,
					     
					     [ '#BQ',
					       
					       [ lambda,
						 [c38_rest, sys_ignored],
						 
						 [ declare,
						   [ignore, sys_ignored]
						 ],
						 
						 [ return_from,
						   ['#COMMA', sys_block_name],
						   ['#COMMA', sys_form]
						 ]
					       ]
					     ]
					   ],
					   
					   [ let,
					     
					     [ 
					       [ sys_expander,
						 
						 [ '#BQ',
						   
						   [ lambda,
						     
						     [ ['#COMMA', sys_form],
						       ['#COMMA', env]
						     ],
						     
						     [ declare,
						       
						       [ ignorable,
							 ['#COMMA', env]
						       ]
						     ],
						     
						     [ block,
						       
						       [ '#COMMA',
							 sys_block_name
						       ],
						       ['#COMMA', sys_body]
						     ]
						   ]
						 ]
					       ]
					     ],
					     
					     [ '#BQ',
					       
					       [ progn,
						 
						 [ sys_record_source_information_for_type,
						   [quote, ['#COMMA', sys_name]],
						   kw_compiler_macro
						 ],
						 
						 [ setf,
						   
						   [ compiler_macro_function,
						     
						     [ quote,
						       ['#COMMA', sys_name]
						     ]
						   ],
						   
						   [ function,
						     ['#COMMA', sys_expander]
						   ]
						 ],
						 [quote, ['#COMMA', sys_name]]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(define_compiler_macro,
			  arglist_info(define_compiler_macro,
				       cl_define_compiler_type_macro,
				       
				       [ sys_name,
					 sys_lambda_list,
					 c38_rest,
					 sys_body
				       ],
				       arginfo{ all:[sys_name, sys_lambda_list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_lambda_list,
							sys_body
						      ],
						opt:0,
						req:[sys_name, sys_lambda_list],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(define_compiler_macro,
			  init_args(2, cl_define_compiler_type_macro))).
*/
/*
; when we encounter an error
*/
/*
; parsing the arguments in the call
*/
/*
; (not in the difinition!), return
*/
/*
; the arguments unmodified -- ie skip the
*/
/*
; transform (see also source-transform.lisp)
*/
/*
;; Adapted from OpenMCL.
*/
/*
(defun compiler-macroexpand-1 (form &optional env)
  (let ((expander nil)
        (new-form nil))
    (if (and (consp form)
             (symbolp (%car form))
             (setq expander (compiler-macro-function (%car form) env)))
        (values (setq new-form (funcall expander form env))
                (neq new-form form))
        (values form
                nil))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:8709 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'compiler-macroexpand-1',[form,'&optional',env],[let,[[expander,[]],['new-form',[]]],[if,[and,[consp,form],[symbolp,['%car',form]],[setq,expander,['compiler-macro-function',['%car',form],env]]],[values,[setq,'new-form',[funcall,expander,form,env]],[neq,'new-form',form]],[values,form,[]]]]])
wl:lambda_def(defun, sys_compiler_macroexpand_1, f_sys_compiler_macroexpand_1, [sys_form, c38_optional, env], [[let, [[sys_expander, []], [sys_new_form, []]], [if, [and, [consp, sys_form], [symbolp, [ext_pf_car, sys_form]], [setq, sys_expander, [compiler_macro_function, [ext_pf_car, sys_form], env]]], [values, [setq, sys_new_form, [funcall, sys_expander, sys_form, env]], [ext_neq, sys_new_form, sys_form]], [values, sys_form, []]]]]).
wl:arglist_info(sys_compiler_macroexpand_1, f_sys_compiler_macroexpand_1, [sys_form, c38_optional, env], arginfo{all:[sys_form, env], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, env], opt:[env], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_compiler_macroexpand_1).

/*

### Compiled:  `SYS::COMPILER-MACROEXPAND-1` 
*/
f_sys_compiler_macroexpand_1(Form_In, RestNKeys, FnResult) :-
	Env36=[bv(sys_form, Form_In), bv(env, Env_In)|Env],
	global_env(Env),
	opt_var(Env, env, Env_In, true, [], 1, RestNKeys),
	LEnv=[bv(sys_expander, []), bv(sys_new_form, [])|Env36],
	get_var(LEnv, sys_form, Form_Get),
	(   is_consp(Form_Get)
	->  get_var(LEnv, sys_form, Form_Get20),
	    f_ext_pf_car(Form_Get20, PredArgResult22),
	    (   is_symbolp(PredArgResult22)
	    ->  get_var(LEnv, sys_form, Form_Get24),
		f_ext_pf_car(Form_Get24, Macro_function_Param),
		get_var(LEnv, env, Env_Get),
		cl_compiler_macro_function(Macro_function_Param,
					   Env_Get,
					   TrueResult),
		set_var(LEnv, sys_expander, TrueResult),
		IFTEST=TrueResult
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, env, Env_Get30),
	    get_var(LEnv, sys_expander, Expander_Get),
	    get_var(LEnv, sys_form, Form_Get29),
	    cl_apply(Expander_Get, [Form_Get29, Env_Get30], TrueResult33),
	    set_var(LEnv, sys_new_form, TrueResult33),
	    get_var(LEnv, sys_form, Form_Get32),
	    get_var(LEnv, sys_new_form, New_form_Get),
	    f_ext_neq(New_form_Get, Form_Get32, Ext_neq_Ret),
	    nb_setval('$mv_return', [TrueResult33, Ext_neq_Ret]),
	    FnResult=TrueResult33
	;   nb_setval('$mv_return', [sys_form, []]),
	    FnResult=sys_form
	).
:- set_opv(f_sys_compiler_macroexpand_1, classof, claz_function),
   set_opv(sys_compiler_macroexpand_1, compile_as, kw_function),
   set_opv(sys_compiler_macroexpand_1, function, f_sys_compiler_macroexpand_1),
   DefunResult=sys_compiler_macroexpand_1.
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand_1,
			  lambda_def(defun,
				     sys_compiler_macroexpand_1,
				     f_sys_compiler_macroexpand_1,
				     [sys_form, c38_optional, env],
				     
				     [ 
				       [ let,
					 [[sys_expander, []], [sys_new_form, []]],
					 
					 [ if,
					   
					   [ and,
					     [consp, sys_form],
					     [symbolp, [ext_pf_car, sys_form]],
					     
					     [ setq,
					       sys_expander,
					       
					       [ compiler_macro_function,
						 [ext_pf_car, sys_form],
						 env
					       ]
					     ]
					   ],
					   
					   [ values,
					     
					     [ setq,
					       sys_new_form,
					       
					       [ funcall,
						 sys_expander,
						 sys_form,
						 env
					       ]
					     ],
					     [ext_neq, sys_new_form, sys_form]
					   ],
					   [values, sys_form, []]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand_1,
			  arglist_info(sys_compiler_macroexpand_1,
				       f_sys_compiler_macroexpand_1,
				       [sys_form, c38_optional, env],
				       arginfo{ all:[sys_form, env],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_form, env],
						opt:[env],
						req:[sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand_1,
			  init_args(1, f_sys_compiler_macroexpand_1))).
*/
/*
(defun compiler-macroexpand (form &optional env)
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expansion exp-p)
          (compiler-macroexpand-1 form env)
        (if exp-p
            (setf form expansion
                  expanded-p t)
            (return))))
    (values form expanded-p)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9083 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'compiler-macroexpand',[form,'&optional',env],[let,[['expanded-p',[]]],[loop,['multiple-value-bind',[expansion,'exp-p'],['compiler-macroexpand-1',form,env],[if,'exp-p',[setf,form,expansion,'expanded-p',t],[return]]]],[values,form,'expanded-p']]])
wl:lambda_def(defun, sys_compiler_macroexpand, f_sys_compiler_macroexpand, [sys_form, c38_optional, env], [[let, [[sys_expanded_p, []]], [loop, [multiple_value_bind, [sys_expansion, sys_exp_p], [sys_compiler_macroexpand_1, sys_form, env], [if, sys_exp_p, [setf, sys_form, sys_expansion, sys_expanded_p, t], [return]]]], [values, sys_form, sys_expanded_p]]]).
wl:arglist_info(sys_compiler_macroexpand, f_sys_compiler_macroexpand, [sys_form, c38_optional, env], arginfo{all:[sys_form, env], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, env], opt:[env], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_compiler_macroexpand).

/*

### Compiled:  `SYS:COMPILER-MACROEXPAND` 
*/
f_sys_compiler_macroexpand(Form_In, RestNKeys, FnResult) :-
	Env16=[bv(sys_form, Form_In), bv(env, Env_In)|Env],
	global_env(Env),
	opt_var(Env, env, Env_In, true, [], 1, RestNKeys),
	LEnv=[bv(sys_expanded_p, [])|Env16],
	cl_loop(
		[ multiple_value_bind,
		  [sys_expansion, sys_exp_p],
		  [sys_compiler_macroexpand_1, sys_form, env],
		  
		  [ if,
		    sys_exp_p,
		    [setf, sys_form, sys_expansion, sys_expanded_p, t],
		    [return]
		  ]
		],
		Loop_Ret),
	get_var(LEnv, sys_expanded_p, Expanded_p_Get),
	nb_setval('$mv_return', [sys_form, Expanded_p_Get]),
	sys_form=FnResult.
:- set_opv(f_sys_compiler_macroexpand, classof, claz_function),
   set_opv(sys_compiler_macroexpand, compile_as, kw_function),
   set_opv(sys_compiler_macroexpand, function, f_sys_compiler_macroexpand),
   DefunResult=sys_compiler_macroexpand.
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand,
			  lambda_def(defun,
				     sys_compiler_macroexpand,
				     f_sys_compiler_macroexpand,
				     [sys_form, c38_optional, env],
				     
				     [ 
				       [ let,
					 [[sys_expanded_p, []]],
					 
					 [ loop,
					   
					   [ multiple_value_bind,
					     [sys_expansion, sys_exp_p],
					     
					     [ sys_compiler_macroexpand_1,
					       sys_form,
					       env
					     ],
					     
					     [ if,
					       sys_exp_p,
					       
					       [ setf,
						 sys_form,
						 sys_expansion,
						 sys_expanded_p,
						 t
					       ],
					       [return]
					     ]
					   ]
					 ],
					 [values, sys_form, sys_expanded_p]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand,
			  arglist_info(sys_compiler_macroexpand,
				       f_sys_compiler_macroexpand,
				       [sys_form, c38_optional, env],
				       arginfo{ all:[sys_form, env],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_form, env],
						opt:[env],
						req:[sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_compiler_macroexpand,
			  init_args(1, f_sys_compiler_macroexpand))).
*/
/*
(export 'defconst)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9396 **********************/
:-lisp_compile_to_prolog(pkg_sys,[export,[quote,defconst]])
:- cl_export(sys_defconst, _Ignored).
/*
(defmacro in-package (name)
  `(%in-package ,(string name)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9416 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'in-package',[name],['#BQ',['%in-package',['#COMMA',[string,name]]]]])
wl:lambda_def(defmacro, in_package, cl_in_package, [sys_name], [progn, ['#BQ', [sys_pf_in_package, ['#COMMA', [string, sys_name]]]]]).
wl:arglist_info(in_package, cl_in_package, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_in_package).

/*

### Compiled:  `CL:IN-PACKAGE` 
*/
cl_in_package(Name_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_name, Name_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_name, Name_Get),
	cl_string(Name_Get, String_Ret),
	[sys_pf_in_package, String_Ret]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_in_package, classof, claz_macro),
   set_opv(in_package, compile_as, kw_operator),
   set_opv(in_package, function, cl_in_package),
   DefMacroResult=in_package.
/*
:- side_effect(assert_lsp(in_package,
			  lambda_def(defmacro,
				     in_package,
				     cl_in_package,
				     [sys_name],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ sys_pf_in_package,
					   ['#COMMA', [string, sys_name]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(in_package,
			  arglist_info(in_package,
				       cl_in_package,
				       [sys_name],
				       arginfo{ all:[sys_name],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_name],
						opt:0,
						req:[sys_name],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(in_package, init_args(exact_only, cl_in_package))).
*/
/*
(defmacro when (test-form &rest body)
  (if (cdr body)
      `(if ,test-form (progn ,@body))
      `(if ,test-form ,(car body))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9478 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,when,['test-form','&rest',body],[if,[cdr,body],['#BQ',[if,['#COMMA','test-form'],[progn,['#BQ-COMMA-ELIPSE',body]]]],['#BQ',[if,['#COMMA','test-form'],['#COMMA',[car,body]]]]]])
wl:lambda_def(defmacro, when, cl_when, [sys_test_form, c38_rest, sys_body], [progn, [if, [cdr, sys_body], ['#BQ', [if, ['#COMMA', sys_test_form], [progn, ['#BQ-COMMA-ELIPSE', sys_body]]]], ['#BQ', [if, ['#COMMA', sys_test_form], ['#COMMA', [car, sys_body]]]]]]).
wl:arglist_info(when, cl_when, [sys_test_form, c38_rest, sys_body], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, sys_body], opt:0, req:[sys_test_form], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(1, cl_when).

/*

### Compiled:  `CL:WHEN` 
*/
cl_when(Test_form_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_test_form, Test_form_In), bv(sys_body, RestNKeys)|CDR],
	global_env(CDR),
	get_var(Env, sys_body, Body_Get),
	cl_cdr(Body_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, sys_body, Body_Get13),
	    get_var(Env, sys_test_form, Test_form_Get),
	    _42698174=[if, Test_form_Get, [progn|Body_Get13]]
	;   get_var(Env, sys_body, Body_Get15),
	    get_var(Env, sys_test_form, Test_form_Get14),
	    cl_car(Body_Get15, Car_Ret),
	    _42698174=[if, Test_form_Get14, Car_Ret]
	),
	_42698174=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_when, classof, claz_macro),
   set_opv(when, compile_as, kw_operator),
   set_opv(when, function, cl_when),
   DefMacroResult=when.
/*
:- side_effect(assert_lsp(when,
			  lambda_def(defmacro,
				     when,
				     cl_when,
				     [sys_test_form, c38_rest, sys_body],
				     
				     [ progn,
				       
				       [ if,
					 [cdr, sys_body],
					 
					 [ '#BQ',
					   
					   [ if,
					     ['#COMMA', sys_test_form],
					     
					     [ progn,
					       ['#BQ-COMMA-ELIPSE', sys_body]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ if,
					     ['#COMMA', sys_test_form],
					     ['#COMMA', [car, sys_body]]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(when,
			  arglist_info(when,
				       cl_when,
				       [sys_test_form, c38_rest, sys_body],
				       arginfo{ all:[sys_test_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_test_form, sys_body],
						opt:0,
						req:[sys_test_form],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(when, init_args(1, cl_when))).
*/
/*
(defmacro unless (test-form &rest body)
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
      `(if (not ,test-form) ,(car body))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9609 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,unless,['test-form','&rest',body],[if,[cdr,body],['#BQ',[if,[not,['#COMMA','test-form']],[progn,['#BQ-COMMA-ELIPSE',body]]]],['#BQ',[if,[not,['#COMMA','test-form']],['#COMMA',[car,body]]]]]])
wl:lambda_def(defmacro, unless, cl_unless, [sys_test_form, c38_rest, sys_body], [progn, [if, [cdr, sys_body], ['#BQ', [if, [not, ['#COMMA', sys_test_form]], [progn, ['#BQ-COMMA-ELIPSE', sys_body]]]], ['#BQ', [if, [not, ['#COMMA', sys_test_form]], ['#COMMA', [car, sys_body]]]]]]).
wl:arglist_info(unless, cl_unless, [sys_test_form, c38_rest, sys_body], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, sys_body], opt:0, req:[sys_test_form], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(1, cl_unless).

/*

### Compiled:  `CL:UNLESS` 
*/
cl_unless(Test_form_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_test_form, Test_form_In), bv(sys_body, RestNKeys)|CDR],
	global_env(CDR),
	get_var(Env, sys_body, Body_Get),
	cl_cdr(Body_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, sys_body, Body_Get13),
	    get_var(Env, sys_test_form, Test_form_Get),
	    _43695258=[if, [not, Test_form_Get], [progn|Body_Get13]]
	;   get_var(Env, sys_body, Body_Get15),
	    get_var(Env, sys_test_form, Test_form_Get14),
	    cl_car(Body_Get15, Car_Ret),
	    _43695258=[if, [not, Test_form_Get14], Car_Ret]
	),
	_43695258=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_unless, classof, claz_macro),
   set_opv(unless, compile_as, kw_operator),
   set_opv(unless, function, cl_unless),
   DefMacroResult=unless.
/*
:- side_effect(assert_lsp(unless,
			  lambda_def(defmacro,
				     unless,
				     cl_unless,
				     [sys_test_form, c38_rest, sys_body],
				     
				     [ progn,
				       
				       [ if,
					 [cdr, sys_body],
					 
					 [ '#BQ',
					   
					   [ if,
					     [not, ['#COMMA', sys_test_form]],
					     
					     [ progn,
					       ['#BQ-COMMA-ELIPSE', sys_body]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ if,
					     [not, ['#COMMA', sys_test_form]],
					     ['#COMMA', [car, sys_body]]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(unless,
			  arglist_info(unless,
				       cl_unless,
				       [sys_test_form, c38_rest, sys_body],
				       arginfo{ all:[sys_test_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_test_form, sys_body],
						opt:0,
						req:[sys_test_form],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(unless, init_args(1, cl_unless))).
*/
/*
(defmacro return (&optional result)
  `(return-from nil ,result))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9754 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,return,['&optional',result],['#BQ',['return-from',[],['#COMMA',result]]]])
wl:lambda_def(defmacro, return, cl_return, [c38_optional, sys_result], [progn, ['#BQ', [return_from, [], ['#COMMA', sys_result]]]]).
wl:arglist_info(return, cl_return, [c38_optional, sys_result], arginfo{all:[sys_result], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_result], opt:[sys_result], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, cl_return).

/*

### Compiled:  `CL:RETURN` 
*/
cl_return(Return_Param, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_result, Result_In)|Opt_var_Param],
	global_env(Opt_var_Param),
	append([], RestNKeys, Return_Param),
	opt_var(Opt_var_Param, sys_result, Result_In, true, [], 1, RestNKeys),
	get_var(Env, sys_result, Result_Get),
	[return_from, [], Result_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_return, classof, claz_macro),
   set_opv(return, compile_as, kw_operator),
   set_opv(return, function, cl_return),
   DefMacroResult=return.
/*
:- side_effect(assert_lsp(return,
			  lambda_def(defmacro,
				     return,
				     cl_return,
				     [c38_optional, sys_result],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ return_from,
					   [],
					   ['#COMMA', sys_result]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(return,
			  arglist_info(return,
				       cl_return,
				       [c38_optional, sys_result],
				       arginfo{ all:[sys_result],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_result],
						opt:[sys_result],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(return, init_args(0, cl_return))).
*/
/*
(defmacro defconstant (name initial-value &optional docstring)
  `(progn
     (record-source-information-for-type ',name :constant)
     (%defconstant ',name ,initial-value ,docstring)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:9821 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defconstant,[name,'initial-value','&optional',docstring],['#BQ',[progn,['record-source-information-for-type',[quote,['#COMMA',name]],':constant'],['%defconstant',[quote,['#COMMA',name]],['#COMMA','initial-value'],['#COMMA',docstring]]]]])
wl:lambda_def(defmacro, defconstant, cl_defconstant, [sys_name, sys_initial_value, c38_optional, sys_docstring], [progn, ['#BQ', [progn, [sys_record_source_information_for_type, [quote, ['#COMMA', sys_name]], kw_constant], [sys_pf_defconstant, [quote, ['#COMMA', sys_name]], ['#COMMA', sys_initial_value], ['#COMMA', sys_docstring]]]]]).
wl:arglist_info(defconstant, cl_defconstant, [sys_name, sys_initial_value, c38_optional, sys_docstring], arginfo{all:[sys_name, sys_initial_value, sys_docstring], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_initial_value, sys_docstring], opt:[sys_docstring], req:[sys_name, sys_initial_value], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_defconstant).

/*

### Compiled:  `CL:DEFCONSTANT` 
*/
cl_defconstant(Name_In, Initial_value_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_name, Name_In), bv(sys_initial_value, Initial_value_In), bv(sys_docstring, Docstring_In)|Opt_var_Param],
	global_env(Opt_var_Param),
	opt_var(Opt_var_Param, sys_docstring, Docstring_In, true, [], 1, RestNKeys),
	get_var(Env, sys_docstring, Docstring_Get),
	get_var(Env, sys_initial_value, Initial_value_Get),
	get_var(Env, sys_name, Name_Get12),
	[progn, [sys_record_source_information_for_type, [quote, Name_Get12], kw_constant], [sys_pf_defconstant, [quote, Name_Get12], Initial_value_Get, Docstring_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defconstant, classof, claz_macro),
   set_opv(defconstant, compile_as, kw_operator),
   set_opv(defconstant, function, cl_defconstant),
   DefMacroResult=defconstant.
/*
:- side_effect(assert_lsp(defconstant,
			  lambda_def(defmacro,
				     defconstant,
				     cl_defconstant,
				     
				     [ sys_name,
				       sys_initial_value,
				       c38_optional,
				       sys_docstring
				     ],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ progn,
					   
					   [ sys_record_source_information_for_type,
					     [quote, ['#COMMA', sys_name]],
					     kw_constant
					   ],
					   
					   [ sys_pf_defconstant,
					     [quote, ['#COMMA', sys_name]],
					     ['#COMMA', sys_initial_value],
					     ['#COMMA', sys_docstring]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defconstant,
			  arglist_info(defconstant,
				       cl_defconstant,
				       
				       [ sys_name,
					 sys_initial_value,
					 c38_optional,
					 sys_docstring
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_initial_value,
						      sys_docstring
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_initial_value,
							sys_docstring
						      ],
						opt:[sys_docstring],
						req:
						    [ sys_name,
						      sys_initial_value
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defconstant, init_args(2, cl_defconstant))).
*/
/*
(defmacro defparameter (name initial-value &optional docstring)
  `(progn
     (record-source-information-for-type ',name :variable)
     (%defparameter ',name ,initial-value ,docstring)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10009 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defparameter,[name,'initial-value','&optional',docstring],['#BQ',[progn,['record-source-information-for-type',[quote,['#COMMA',name]],':variable'],['%defparameter',[quote,['#COMMA',name]],['#COMMA','initial-value'],['#COMMA',docstring]]]]])
wl:lambda_def(defmacro, defparameter, cl_defparameter, [sys_name, sys_initial_value, c38_optional, sys_docstring], [progn, ['#BQ', [progn, [sys_record_source_information_for_type, [quote, ['#COMMA', sys_name]], kw_variable], [sys_pf_defparameter, [quote, ['#COMMA', sys_name]], ['#COMMA', sys_initial_value], ['#COMMA', sys_docstring]]]]]).
wl:arglist_info(defparameter, cl_defparameter, [sys_name, sys_initial_value, c38_optional, sys_docstring], arginfo{all:[sys_name, sys_initial_value, sys_docstring], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_initial_value, sys_docstring], opt:[sys_docstring], req:[sys_name, sys_initial_value], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_defparameter).

/*

### Compiled:  `CL:DEFPARAMETER` 
*/
cl_defparameter(Name_In, Initial_value_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_name, Name_In), bv(sys_initial_value, Initial_value_In), bv(sys_docstring, Docstring_In)|Opt_var_Param],
	global_env(Opt_var_Param),
	opt_var(Opt_var_Param, sys_docstring, Docstring_In, true, [], 1, RestNKeys),
	get_var(Env, sys_docstring, Docstring_Get),
	get_var(Env, sys_initial_value, Initial_value_Get),
	get_var(Env, sys_name, Name_Get12),
	[progn, [sys_record_source_information_for_type, [quote, Name_Get12], kw_variable], [sys_pf_defparameter, [quote, Name_Get12], Initial_value_Get, Docstring_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defparameter, classof, claz_macro),
   set_opv(defparameter, compile_as, kw_operator),
   set_opv(defparameter, function, cl_defparameter),
   DefMacroResult=defparameter.
/*
:- side_effect(assert_lsp(defparameter,
			  lambda_def(defmacro,
				     defparameter,
				     cl_defparameter,
				     
				     [ sys_name,
				       sys_initial_value,
				       c38_optional,
				       sys_docstring
				     ],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ progn,
					   
					   [ sys_record_source_information_for_type,
					     [quote, ['#COMMA', sys_name]],
					     kw_variable
					   ],
					   
					   [ sys_pf_defparameter,
					     [quote, ['#COMMA', sys_name]],
					     ['#COMMA', sys_initial_value],
					     ['#COMMA', sys_docstring]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defparameter,
			  arglist_info(defparameter,
				       cl_defparameter,
				       
				       [ sys_name,
					 sys_initial_value,
					 c38_optional,
					 sys_docstring
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_initial_value,
						      sys_docstring
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_initial_value,
							sys_docstring
						      ],
						opt:[sys_docstring],
						req:
						    [ sys_name,
						      sys_initial_value
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defparameter, init_args(2, cl_defparameter))).
*/
/*
(defmacro truly-the (type value)
  `(the ,type ,value))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10199 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'truly-the',[type,value],['#BQ',[the,['#COMMA',type],['#COMMA',value]]]])
wl:lambda_def(defmacro, ext_truly_the, f_ext_truly_the, [type, sys_value], [progn, ['#BQ', [the, ['#COMMA', type], ['#COMMA', sys_value]]]]).
wl:arglist_info(ext_truly_the, f_ext_truly_the, [type, sys_value], arginfo{all:[type, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[type, sys_value], opt:0, req:[type, sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_truly_the).

/*

### Compiled:  `EXT:TRULY-THE` 
*/
f_ext_truly_the(Type_In, Value_In, FnResult) :-
	nop(defmacro),
	Env=[bv(type, Type_In), bv(sys_value, Value_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_value, Value_Get),
	get_var(Env, type, Type_Get),
	[the, Type_Get, Value_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_truly_the, classof, claz_macro),
   set_opv(ext_truly_the, compile_as, kw_operator),
   set_opv(ext_truly_the, function, f_ext_truly_the),
   DefMacroResult=ext_truly_the.
/*
:- side_effect(assert_lsp(ext_truly_the,
			  lambda_def(defmacro,
				     ext_truly_the,
				     f_ext_truly_the,
				     [type, sys_value],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ the,
					   ['#COMMA', type],
					   ['#COMMA', sys_value]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ext_truly_the,
			  arglist_info(ext_truly_the,
				       f_ext_truly_the,
				       [type, sys_value],
				       arginfo{ all:[type, sys_value],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[type, sys_value],
						opt:0,
						req:[type, sys_value],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ext_truly_the, init_args(exact_only, f_ext_truly_the))).
*/
/*
(defmacro %car (x)
  `(car (truly-the cons ,x)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10256 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'%car',[x],['#BQ',[car,['truly-the',cons,['#COMMA',x]]]]])
wl:lambda_def(defmacro, ext_pf_car, f_ext_pf_car, [sys_x], [progn, ['#BQ', [car, [ext_truly_the, cons, ['#COMMA', sys_x]]]]]).
wl:arglist_info(ext_pf_car, f_ext_pf_car, [sys_x], arginfo{all:[sys_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x], opt:0, req:[sys_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_pf_car).

/*

### Compiled:  `EXT:%CAR` 
*/
f_ext_pf_car(X_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_x, X_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_x, X_Get),
	[car, [ext_truly_the, cons, X_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_pf_car, classof, claz_macro),
   set_opv(ext_pf_car, compile_as, kw_operator),
   set_opv(ext_pf_car, function, f_ext_pf_car),
   DefMacroResult=ext_pf_car.
/*
:- side_effect(assert_lsp(ext_pf_car,
			  lambda_def(defmacro,
				     ext_pf_car,
				     f_ext_pf_car,
				     [sys_x],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ car,
					   
					   [ ext_truly_the,
					     cons,
					     ['#COMMA', sys_x]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ext_pf_car,
			  arglist_info(ext_pf_car,
				       f_ext_pf_car,
				       [sys_x],
				       arginfo{ all:[sys_x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x],
						opt:0,
						req:[sys_x],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ext_pf_car, init_args(exact_only, f_ext_pf_car))).
*/
/*
(defmacro %cdr (x)
  `(cdr (truly-the cons ,x)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10306 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'%cdr',[x],['#BQ',[cdr,['truly-the',cons,['#COMMA',x]]]]])
wl:lambda_def(defmacro, ext_pf_cdr, f_ext_pf_cdr, [sys_x], [progn, ['#BQ', [cdr, [ext_truly_the, cons, ['#COMMA', sys_x]]]]]).
wl:arglist_info(ext_pf_cdr, f_ext_pf_cdr, [sys_x], arginfo{all:[sys_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x], opt:0, req:[sys_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_pf_cdr).

/*

### Compiled:  `EXT:%CDR` 
*/
f_ext_pf_cdr(X_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_x, X_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_x, X_Get),
	[cdr, [ext_truly_the, cons, X_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_pf_cdr, classof, claz_macro),
   set_opv(ext_pf_cdr, compile_as, kw_operator),
   set_opv(ext_pf_cdr, function, f_ext_pf_cdr),
   DefMacroResult=ext_pf_cdr.
/*
:- side_effect(assert_lsp(ext_pf_cdr,
			  lambda_def(defmacro,
				     ext_pf_cdr,
				     f_ext_pf_cdr,
				     [sys_x],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ cdr,
					   
					   [ ext_truly_the,
					     cons,
					     ['#COMMA', sys_x]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ext_pf_cdr,
			  arglist_info(ext_pf_cdr,
				       f_ext_pf_cdr,
				       [sys_x],
				       arginfo{ all:[sys_x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x],
						opt:0,
						req:[sys_x],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ext_pf_cdr, init_args(exact_only, f_ext_pf_cdr))).
*/
/*
(defmacro %cadr (x)
  `(%car (%cdr ,x)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10356 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'%cadr',[x],['#BQ',['%car',['%cdr',['#COMMA',x]]]]])
wl:lambda_def(defmacro, ext_pf_cadr, f_ext_pf_cadr, [sys_x], [progn, ['#BQ', [ext_pf_car, [ext_pf_cdr, ['#COMMA', sys_x]]]]]).
wl:arglist_info(ext_pf_cadr, f_ext_pf_cadr, [sys_x], arginfo{all:[sys_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x], opt:0, req:[sys_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_pf_cadr).

/*

### Compiled:  `EXT:%CADR` 
*/
f_ext_pf_cadr(X_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_x, X_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_x, X_Get),
	[ext_pf_car, [ext_pf_cdr, X_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_pf_cadr, classof, claz_macro),
   set_opv(ext_pf_cadr, compile_as, kw_operator),
   set_opv(ext_pf_cadr, function, f_ext_pf_cadr),
   DefMacroResult=ext_pf_cadr.
/*
:- side_effect(assert_lsp(ext_pf_cadr,
			  lambda_def(defmacro,
				     ext_pf_cadr,
				     f_ext_pf_cadr,
				     [sys_x],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ ext_pf_car,
					   [ext_pf_cdr, ['#COMMA', sys_x]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ext_pf_cadr,
			  arglist_info(ext_pf_cadr,
				       f_ext_pf_cadr,
				       [sys_x],
				       arginfo{ all:[sys_x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x],
						opt:0,
						req:[sys_x],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ext_pf_cadr, init_args(exact_only, f_ext_pf_cadr))).
*/
/*
(defmacro %caddr (x)
  `(%car (%cdr (%cdr ,x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10398 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'%caddr',[x],['#BQ',['%car',['%cdr',['%cdr',['#COMMA',x]]]]]])
wl:lambda_def(defmacro, ext_pf_caddr, f_ext_pf_caddr, [sys_x], [progn, ['#BQ', [ext_pf_car, [ext_pf_cdr, [ext_pf_cdr, ['#COMMA', sys_x]]]]]]).
wl:arglist_info(ext_pf_caddr, f_ext_pf_caddr, [sys_x], arginfo{all:[sys_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x], opt:0, req:[sys_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_pf_caddr).

/*

### Compiled:  `EXT:%CADDR` 
*/
f_ext_pf_caddr(X_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_x, X_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_x, X_Get),
	[ext_pf_car, [ext_pf_cdr, [ext_pf_cdr, X_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_pf_caddr, classof, claz_macro),
   set_opv(ext_pf_caddr, compile_as, kw_operator),
   set_opv(ext_pf_caddr, function, f_ext_pf_caddr),
   DefMacroResult=ext_pf_caddr.
/*
:- side_effect(assert_lsp(ext_pf_caddr,
			  lambda_def(defmacro,
				     ext_pf_caddr,
				     f_ext_pf_caddr,
				     [sys_x],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ ext_pf_car,
					   
					   [ ext_pf_cdr,
					     [ext_pf_cdr, ['#COMMA', sys_x]]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ext_pf_caddr,
			  arglist_info(ext_pf_caddr,
				       f_ext_pf_caddr,
				       [sys_x],
				       arginfo{ all:[sys_x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x],
						opt:0,
						req:[sys_x],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ext_pf_caddr, init_args(exact_only, f_ext_pf_caddr))).
*/
/*
(defmacro prog1 (first-form &rest forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10448 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,prog1,['first-form','&rest',forms],[let,[[result,[gensym]]],['#BQ',[let,[[['#COMMA',result],['#COMMA','first-form']]],['#BQ-COMMA-ELIPSE',forms],['#COMMA',result]]]]])
wl:lambda_def(defmacro, prog1, cl_prog1, [sys_first_form, c38_rest, forms], [progn, [let, [[sys_result, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_result], ['#COMMA', sys_first_form]]], ['#BQ-COMMA-ELIPSE', forms], ['#COMMA', sys_result]]]]]).
wl:arglist_info(prog1, cl_prog1, [sys_first_form, c38_rest, forms], arginfo{all:[sys_first_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, forms], opt:0, req:[sys_first_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_prog1).

/*

### Compiled:  `CL:PROG1` 
*/
cl_prog1(First_form_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_first_form, First_form_In), bv(forms, RestNKeys)|CDR],
	global_env(CDR),
	cl_gensym(Result_Init),
	LEnv=[bv(sys_result, Result_Init)|Env],
	get_var(LEnv, forms, Forms_Get),
	get_var(LEnv, sys_first_form, First_form_Get),
	get_var(LEnv, sys_result, Result_Get16),
	bq_append([[[Result_Get16, First_form_Get]]|Forms_Get],
		  [Result_Get16],
		  Bq_append_Ret),
	[let|Bq_append_Ret]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog1, classof, claz_macro),
   set_opv(prog1, compile_as, kw_operator),
   set_opv(prog1, function, cl_prog1),
   DefMacroResult=prog1.
/*
:- side_effect(assert_lsp(prog1,
			  lambda_def(defmacro,
				     prog1,
				     cl_prog1,
				     [sys_first_form, c38_rest, forms],
				     
				     [ progn,
				       
				       [ let,
					 [[sys_result, [gensym]]],
					 
					 [ '#BQ',
					   
					   [ let,
					     
					     [ 
					       [ ['#COMMA', sys_result],
						 ['#COMMA', sys_first_form]
					       ]
					     ],
					     ['#BQ-COMMA-ELIPSE', forms],
					     ['#COMMA', sys_result]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(prog1,
			  arglist_info(prog1,
				       cl_prog1,
				       [sys_first_form, c38_rest, forms],
				       arginfo{ all:[sys_first_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_first_form, forms],
						opt:0,
						req:[sys_first_form],
						rest:[forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(prog1, init_args(1, cl_prog1))).
*/
/*
(defmacro prog2 (first-form second-form &rest forms)
  `(prog1 (progn ,first-form ,second-form) ,@forms))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10584 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,prog2,['first-form','second-form','&rest',forms],['#BQ',[prog1,[progn,['#COMMA','first-form'],['#COMMA','second-form']],['#BQ-COMMA-ELIPSE',forms]]]])
wl:lambda_def(defmacro, prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], [progn, ['#BQ', [prog1, [progn, ['#COMMA', sys_first_form], ['#COMMA', sys_second_form]], ['#BQ-COMMA-ELIPSE', forms]]]]).
wl:arglist_info(prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], arginfo{all:[sys_first_form, sys_second_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, sys_second_form, forms], opt:0, req:[sys_first_form, sys_second_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(2, cl_prog2).

/*

### Compiled:  `CL:PROG2` 
*/
cl_prog2(First_form_In, Second_form_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_first_form, First_form_In), bv(sys_second_form, Second_form_In), bv(forms, RestNKeys)|CDR],
	global_env(CDR),
	get_var(Env, forms, Forms_Get),
	get_var(Env, sys_first_form, First_form_Get),
	get_var(Env, sys_second_form, Second_form_Get),
	[prog1, [progn, First_form_Get, Second_form_Get]|Forms_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog2, classof, claz_macro),
   set_opv(prog2, compile_as, kw_operator),
   set_opv(prog2, function, cl_prog2),
   DefMacroResult=prog2.
/*
:- side_effect(assert_lsp(prog2,
			  lambda_def(defmacro,
				     prog2,
				     cl_prog2,
				     
				     [ sys_first_form,
				       sys_second_form,
				       c38_rest,
				       forms
				     ],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ prog1,
					   
					   [ progn,
					     ['#COMMA', sys_first_form],
					     ['#COMMA', sys_second_form]
					   ],
					   ['#BQ-COMMA-ELIPSE', forms]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(prog2,
			  arglist_info(prog2,
				       cl_prog2,
				       
				       [ sys_first_form,
					 sys_second_form,
					 c38_rest,
					 forms
				       ],
				       arginfo{ all:
						    [ sys_first_form,
						      sys_second_form
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_first_form,
							sys_second_form,
							forms
						      ],
						opt:0,
						req:
						    [ sys_first_form,
						      sys_second_form
						    ],
						rest:[forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(prog2, init_args(2, cl_prog2))).
*/
/*
(defmacro psetq (&environment env &rest args)
  (do ((l args (cddr l))
       (forms nil)
       (bindings nil))
    ((endp l) (list* 'let* (reverse bindings) (reverse (cons nil forms))))
    (if (and (symbolp (car l))
             (eq (car l) (macroexpand-1 (car l) env)))
        (let ((sym (gensym)))
          (push (list sym (cadr l)) bindings)
          (push (list 'setq (car l) sym) forms))
        (multiple-value-bind
              (dummies vals newval setter getter)
            (get-setf-expansion (macroexpand-1 (car l) env) env)
          (declare (ignore getter))
          (do ((d dummies (cdr d))
               (v vals (cdr v)))
              ((null d))
            (push (list (car d) (car v)) bindings))
          (push (list (car newval) (cadr l)) bindings)
          (push setter forms)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:10692 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,psetq,['&environment',env,'&rest',args],[do,[[l,args,[cddr,l]],[forms,[]],[bindings,[]]],[[endp,l],['list*',[quote,'let*'],[reverse,bindings],[reverse,[cons,[],forms]]]],[if,[and,[symbolp,[car,l]],[eq,[car,l],['macroexpand-1',[car,l],env]]],[let,[[sym,[gensym]]],[push,[list,sym,[cadr,l]],bindings],[push,[list,[quote,setq],[car,l],sym],forms]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',['macroexpand-1',[car,l],env],env],[declare,[ignore,getter]],[do,[[d,dummies,[cdr,d]],[v,vals,[cdr,v]]],[[null,d]],[push,[list,[car,d],[car,v]],bindings]],[push,[list,[car,newval],[cadr,l]],bindings],[push,setter,forms]]]]])
wl:lambda_def(defmacro, psetq, cl_psetq, [c38_environment, env, c38_rest, args], [progn, [do, [[sys_l, args, [cddr, sys_l]], [forms, []], [bindings, []]], [[endp, sys_l], [list_xx, [quote, let_xx], [reverse, bindings], [reverse, [cons, [], forms]]]], [if, [and, [symbolp, [car, sys_l]], [eq, [car, sys_l], [macroexpand_1, [car, sys_l], env]]], [let, [[sys_sym, [gensym]]], [push, [list, sys_sym, [cadr, sys_l]], bindings], [push, [list, [quote, setq], [car, sys_l], sys_sym], forms]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, [macroexpand_1, [car, sys_l], env], env], [declare, [ignore, sys_getter]], [do, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]]], [[null, sys_d]], [push, [list, [car, sys_d], [car, sys_v]], bindings]], [push, [list, [car, sys_newval], [cadr, sys_l]], bindings], [push, sys_setter, forms]]]]]).
wl: declared(cl_psetq, env_arg1).

wl:arglist_info(psetq, cl_psetq, [c38_environment, env, c38_rest, args], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[environment, rest], env:[env], key:0, names:[env, args], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, cl_psetq).

/*

### Compiled:  `CL:PSETQ` 
*/
cl_psetq(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(args, RestNKeys)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	catch(( ( get_var(Env, args, Args_Get),
		  AEnv=[bv(sys_l, Args_Get), bv(forms, []), bv(bindings, [])|Env],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_3), get_var(AEnv, sys_l, L_Get87), (is_endp(L_Get87)->get_var(AEnv, bindings, Bindings_Get92), cl_reverse(Bindings_Get92, Reverse_Ret), get_var(AEnv, forms, Forms_Get93), Reverse_Param=[[]|Forms_Get93], cl_reverse(Reverse_Param, Reverse_Ret171), cl_list_xx(let_xx, Reverse_Ret, Reverse_Ret171, RetResult90), throw(block_exit([], RetResult90)), _TBResult=ThrowResult91;get_var(AEnv, sys_l, L_Get98), cl_car(L_Get98, PredArgResult100), (is_symbolp(PredArgResult100)->get_var(AEnv, sys_l, L_Get101), cl_car(L_Get101, Eq_Param), get_var(AEnv, sys_l, L_Get102), cl_car(L_Get102, Car_Ret), get_var(AEnv, env, Env_Get103), cl_macroexpand_1([Car_Ret, Env_Get103], Macroexpand_1_Ret), cl_eq(Eq_Param, Macroexpand_1_Ret, TrueResult104), IFTEST95=TrueResult104;IFTEST95=[]), (IFTEST95\==[]->cl_gensym(Sym_Init108), LEnv107=[bv(sys_sym, Sym_Init108)|AEnv], cl_push([list, sys_sym, [cadr, sys_l]], bindings, [], Push_Ret), cl_push([list, [quote, setq], [car, sys_l], sys_sym], forms, [], LetResult106), _53917960=LetResult106;LEnv111=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv], get_var(LEnv111, sys_l, L_Get112), cl_car(L_Get112, Car_Ret175), get_var(LEnv111, env, Env_Get113), cl_macroexpand_1([Car_Ret175, Env_Get113], Setf_expansion_Param), get_var(LEnv111, env, Env_Get114), cl_get_setf_expansion(Setf_expansion_Param, Env_Get114, Setf_expansion_Ret), setq_from_values(LEnv111, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter]), cl_declare([ignore, sys_getter], Declare_Ret), get_var(LEnv111, sys_dummies, Dummies_Get118), get_var(LEnv111, sys_vals, Vals_Get119), BlockExitEnv=[bv(sys_d, Dummies_Get118), bv(sys_v, Vals_Get119)|LEnv111], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_5), get_var(BlockExitEnv, sys_d, IFTEST136), (IFTEST136==[]->throw(block_exit([], [])), _TBResult122=ThrowResult140;cl_push([list, [car, sys_d], [car, sys_v]], bindings, [], Push_Ret178), get_var(BlockExitEnv, sys_d, D_Get142), cl_cdr(D_Get142, D), get_var(BlockExitEnv, sys_v, V_Get143), cl_cdr(V_Get143, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_5, BlockExitEnv), _TBResult122=_GORES144)), [addr(addr_tagbody_5_do_label_5, do_label_5, '$unused', BlockExitEnv128,  (get_var(BlockExitEnv128, sys_d, IFTEST123), (IFTEST123==[]->throw(block_exit([], [])), _TBResult122=ThrowResult127;cl_push([list, [car, sys_d], [car, sys_v]], bindings, [], Push_Ret179), get_var(BlockExitEnv128, sys_d, D_Get129), cl_cdr(D_Get129, Cdr_Ret), get_var(BlockExitEnv128, sys_v, V_Get130), cl_cdr(V_Get130, Cdr_Ret181), set_var(BlockExitEnv128, sys_d, Cdr_Ret), set_var(BlockExitEnv128, sys_v, Cdr_Ret181), goto(do_label_5, BlockExitEnv128), _TBResult122=_GORES131)))]), []=LetResult116), block_exit([], LetResult116), true), cl_push([list, [car, sys_newval], [cadr, sys_l]], bindings, [], Push_Ret182), cl_push(sys_setter, forms, [], LetResult110), _53917960=LetResult110), get_var(AEnv, sys_l, L_Get151), cl_cddr(L_Get151, L), set_var(AEnv, sys_l, L), goto(do_label_3, AEnv), _TBResult=_GORES152)),
					  
					  [ addr(addr_tagbody_3_do_label_3,
						 do_label_3,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_l, L_Get), (is_endp(L_Get)->get_var(AEnv, bindings, Reverse_Param164), cl_reverse(Reverse_Param164, Reverse_Ret183), get_var(AEnv, forms, Get_var_Ret), Reverse_Param165=[[]|Get_var_Ret], cl_reverse(Reverse_Param165, Reverse_Ret185), cl_list_xx(let_xx, Reverse_Ret183, Reverse_Ret185, List_xx_Ret), throw(block_exit([], List_xx_Ret)), _56113716=ThrowResult;get_var(AEnv, sys_l, L_Get27), cl_car(L_Get27, PredArgResult29), (is_symbolp(PredArgResult29)->get_var(AEnv, sys_l, L_Get30), cl_car(L_Get30, Eq_Param166), get_var(AEnv, sys_l, L_Get31), cl_car(L_Get31, Car_Ret187), get_var(AEnv, env, Get_var_Ret188), cl_macroexpand_1([Car_Ret187, Get_var_Ret188], Macroexpand_1_Ret189), cl_eq(Eq_Param166, Macroexpand_1_Ret189, Eq_Ret), IFTEST24=Eq_Ret;IFTEST24=[]), (IFTEST24\==[]->cl_gensym(Gensym_Ret), LEnv36=[bv(sys_sym, Gensym_Ret)|AEnv], cl_push([list, sys_sym, [cadr, sys_l]], bindings, [], Push_Ret192), cl_push([list, [quote, setq], [car, sys_l], sys_sym], forms, [], LetResult35), _56114088=LetResult35;LEnv40=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv], get_var(LEnv40, sys_l, L_Get41), cl_car(L_Get41, Car_Ret193), get_var(LEnv40, env, Env_Get42), cl_macroexpand_1([Car_Ret193, Env_Get42], Setf_expansion_Param167), get_var(LEnv40, env, Env_Get43), cl_get_setf_expansion(Setf_expansion_Param167, Env_Get43, Setf_expansion_Ret194), setq_from_values(LEnv40, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter]), cl_declare([ignore, sys_getter], Declare_Ret195), get_var(LEnv40, sys_dummies, Dummies_Get), get_var(LEnv40, sys_vals, Vals_Get), BlockExitEnv=[bv(sys_d, Dummies_Get), bv(sys_v, Vals_Get)|LEnv40], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_4), get_var(BlockExitEnv, sys_d, IFTEST65), (IFTEST65==[]->throw(block_exit([], [])), _TBResult51=ThrowResult69;cl_push([list, [car, sys_d], [car, sys_v]], bindings, [], Push_Ret196), get_var(BlockExitEnv, sys_d, D_Get71), cl_cdr(D_Get71, Cdr_Ret197), get_var(BlockExitEnv, sys_v, V_Get72), cl_cdr(V_Get72, Cdr_Ret198), set_var(BlockExitEnv, sys_d, Cdr_Ret197), set_var(BlockExitEnv, sys_v, Cdr_Ret198), goto(do_label_4, BlockExitEnv), _TBResult51=_GORES73)), [addr(addr_tagbody_4_do_label_4, do_label_4, '$unused', BlockExitEnv57,  (get_var(BlockExitEnv57, sys_d, IFTEST52), (IFTEST52==[]->throw(block_exit([], [])), _TBResult51=ThrowResult56;cl_push([list, [car, sys_d], [car, sys_v]], bindings, [], Push_Ret199), get_var(BlockExitEnv57, sys_d, D_Get58), cl_cdr(D_Get58, Cdr_Ret200), get_var(BlockExitEnv57, sys_v, Cdr_Param), cl_cdr(Cdr_Param, Cdr_Ret201), set_var(BlockExitEnv57, sys_d, Cdr_Ret200), set_var(BlockExitEnv57, sys_v, Cdr_Ret201), goto(do_label_4, BlockExitEnv57), _TBResult51=_GORES)))]), []=LetResult45), block_exit([], LetResult45), true), cl_push([list, [car, sys_newval], [cadr, sys_l]], bindings, [], Push_Ret202), cl_push(sys_setter, forms, [], LetResult39), _56114088=LetResult39), get_var(AEnv, sys_l, L_Get80), cl_cddr(L_Get80, Cddr_Ret), set_var(AEnv, sys_l, Cddr_Ret), goto(do_label_3, AEnv), _56113716=_GORES81)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=MFResult
	      ),
	      block_exit(psetq, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_psetq, classof, claz_macro),
   set_opv(psetq, compile_as, kw_operator),
   set_opv(psetq, function, cl_psetq),
   DefMacroResult=psetq.
/*
:- side_effect(assert_lsp(psetq,
			  lambda_def(defmacro,
				     psetq,
				     cl_psetq,
				     [c38_environment, env, c38_rest, args],
				     
				     [ progn,
				       
				       [ do,
					 
					 [ [sys_l, args, [cddr, sys_l]],
					   [forms, []],
					   [bindings, []]
					 ],
					 
					 [ [endp, sys_l],
					   
					   [ list_xx,
					     [quote, let_xx],
					     [reverse, bindings],
					     [reverse, [cons, [], forms]]
					   ]
					 ],
					 
					 [ if,
					   
					   [ and,
					     [symbolp, [car, sys_l]],
					     
					     [ eq,
					       [car, sys_l],
					       [macroexpand_1, [car, sys_l], env]
					     ]
					   ],
					   
					   [ let,
					     [[sys_sym, [gensym]]],
					     
					     [ push,
					       [list, sys_sym, [cadr, sys_l]],
					       bindings
					     ],
					     
					     [ push,
					       
					       [ list,
						 [quote, setq],
						 [car, sys_l],
						 sys_sym
					       ],
					       forms
					     ]
					   ],
					   
					   [ multiple_value_bind,
					     
					     [ sys_dummies,
					       sys_vals,
					       sys_newval,
					       sys_setter,
					       sys_getter
					     ],
					     
					     [ get_setf_expansion,
					       [macroexpand_1, [car, sys_l], env],
					       env
					     ],
					     [declare, [ignore, sys_getter]],
					     
					     [ do,
					       
					       [ 
						 [ sys_d,
						   sys_dummies,
						   [cdr, sys_d]
						 ],
						 [sys_v, sys_vals, [cdr, sys_v]]
					       ],
					       [[null, sys_d]],
					       
					       [ push,
						 
						 [ list,
						   [car, sys_d],
						   [car, sys_v]
						 ],
						 bindings
					       ]
					     ],
					     
					     [ push,
					       
					       [ list,
						 [car, sys_newval],
						 [cadr, sys_l]
					       ],
					       bindings
					     ],
					     [push, sys_setter, forms]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(psetq,
			  arglist_info(psetq,
				       cl_psetq,
				       [c38_environment, env, c38_rest, args],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment, rest],
						env:[env],
						key:0,
						names:[env, args],
						opt:0,
						req:0,
						rest:[args],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(psetq, init_args(0, cl_psetq))).
*/
/*
(defmacro time (form)
  `(%time #'(lambda () ,form)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:11506 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,time,[form],['#BQ',['%time',function([lambda,[],['#COMMA',form]])]]])
wl:lambda_def(defmacro, time, cl_time, [sys_form], [progn, ['#BQ', [sys_pf_time, function([lambda, [], ['#COMMA', sys_form]])]]]).
wl:arglist_info(time, cl_time, [sys_form], arginfo{all:[sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form], opt:0, req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_time).

/*

### Compiled:  `CL:TIME` 
*/
cl_time(Form_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_form, Form_In)|CDR],
	global_env(CDR),
	[sys_pf_time, function([lambda, [], ['#COMMA', sys_form]])]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_time, classof, claz_macro),
   set_opv(time, compile_as, kw_operator),
   set_opv(time, function, cl_time),
   DefMacroResult=time.
/*
:- side_effect(assert_lsp(time,
			  lambda_def(defmacro,
				     time,
				     cl_time,
				     [sys_form],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ sys_pf_time,
					   function(
						    [ lambda,
						      [],
						      ['#COMMA', sys_form]
						    ])
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(time,
			  arglist_info(time,
				       cl_time,
				       [sys_form],
				       arginfo{ all:[sys_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_form],
						opt:0,
						req:[sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(time, init_args(exact_only, cl_time))).
*/
/*
(defmacro with-open-stream (&rest args)
  (let ((var (caar args))
        (stream (cadar args))
        (forms (cdr args))
        (abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       (unwind-protect
        (multiple-value-prog1
         (progn ,@forms)
         (setq ,abortp nil))
        (when ,var
          (close ,var :abort ,abortp))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:11561 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'with-open-stream',['&rest',args],[let,[[var,[caar,args]],[stream,[cadar,args]],[forms,[cdr,args]],[abortp,[gensym]]],['#BQ',[let,[[['#COMMA',var],['#COMMA',stream]],[['#COMMA',abortp],t]],['unwind-protect',['multiple-value-prog1',[progn,['#BQ-COMMA-ELIPSE',forms]],[setq,['#COMMA',abortp],[]]],[when,['#COMMA',var],[close,['#COMMA',var],':abort',['#COMMA',abortp]]]]]]]])
wl:lambda_def(defmacro, with_open_stream, cl_with_open_stream, [c38_rest, args], [progn, [let, [[sys_var, [caar, args]], [stream, [cadar, args]], [forms, [cdr, args]], [sys_abortp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_var], ['#COMMA', stream]], [['#COMMA', sys_abortp], t]], [unwind_protect, [multiple_value_prog1, [progn, ['#BQ-COMMA-ELIPSE', forms]], [setq, ['#COMMA', sys_abortp], []]], [when, ['#COMMA', sys_var], [close, ['#COMMA', sys_var], kw_abort, ['#COMMA', sys_abortp]]]]]]]]).
wl:arglist_info(with_open_stream, cl_with_open_stream, [c38_rest, args], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[args], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, cl_with_open_stream).

/*

### Compiled:  `CL:WITH-OPEN-STREAM` 
*/
cl_with_open_stream(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(args, RestNKeys)|CDR],
	global_env(CDR),
	get_var(Env, args, Args_Get),
	cl_caar(Args_Get, Var_Init),
	get_var(Env, args, Args_Get12),
	cl_cadar(Args_Get12, Stream_Init),
	get_var(Env, args, Args_Get13),
	cl_cdr(Args_Get13, Forms_Init),
	cl_gensym(Abortp_Init),
	LEnv=[bv(sys_var, Var_Init), bv(stream, Stream_Init), bv(forms, Forms_Init), bv(sys_abortp, Abortp_Init)|Env],
	get_var(LEnv, forms, Forms_Get),
	get_var(LEnv, stream, Stream_Get),
	get_var(LEnv, sys_abortp, Abortp_Get22),
	get_var(LEnv, sys_var, Var_Get23),
	[let, [[Var_Get23, Stream_Get], [Abortp_Get22, t]], [unwind_protect, [multiple_value_prog1, [progn|Forms_Get], [setq, Abortp_Get22, []]], [when, Var_Get23, [close, Var_Get23, kw_abort, Abortp_Get22]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_with_open_stream, classof, claz_macro),
   set_opv(with_open_stream, compile_as, kw_operator),
   set_opv(with_open_stream, function, cl_with_open_stream),
   DefMacroResult=with_open_stream.
/*
:- side_effect(assert_lsp(with_open_stream,
			  lambda_def(defmacro,
				     with_open_stream,
				     cl_with_open_stream,
				     [c38_rest, args],
				     
				     [ progn,
				       
				       [ let,
					 
					 [ [sys_var, [caar, args]],
					   [stream, [cadar, args]],
					   [forms, [cdr, args]],
					   [sys_abortp, [gensym]]
					 ],
					 
					 [ '#BQ',
					   
					   [ let,
					     
					     [ 
					       [ ['#COMMA', sys_var],
						 ['#COMMA', stream]
					       ],
					       [['#COMMA', sys_abortp], t]
					     ],
					     
					     [ unwind_protect,
					       
					       [ multiple_value_prog1,
						 
						 [ progn,
						   ['#BQ-COMMA-ELIPSE', forms]
						 ],
						 
						 [ setq,
						   ['#COMMA', sys_abortp],
						   []
						 ]
					       ],
					       
					       [ when,
						 ['#COMMA', sys_var],
						 
						 [ close,
						   ['#COMMA', sys_var],
						   kw_abort,
						   ['#COMMA', sys_abortp]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(with_open_stream,
			  arglist_info(with_open_stream,
				       cl_with_open_stream,
				       [c38_rest, args],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[args],
						opt:0,
						req:0,
						rest:[args],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(with_open_stream, init_args(0, cl_with_open_stream))).
*/
/*
(defun ansi-loop (exps)
  (let ((*warn-on-redefinition* nil))
    (require 'loop))
  (fmakunbound 'ansi-loop)
  `(loop ,@exps))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:11924 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'ansi-loop',[exps],[let,[['*warn-on-redefinition*',[]]],[require,[quote,loop]]],[fmakunbound,[quote,'ansi-loop']],['#BQ',[loop,['#BQ-COMMA-ELIPSE',exps]]]])
wl:lambda_def(defun, sys_ansi_loop, f_sys_ansi_loop, [sys_exps], [[let, [[ext_xx_warn_on_redefinition_xx, []]], [require, [quote, loop]]], [fmakunbound, [quote, sys_ansi_loop]], ['#BQ', [loop, ['#BQ-COMMA-ELIPSE', sys_exps]]]]).
wl:arglist_info(sys_ansi_loop, f_sys_ansi_loop, [sys_exps], arginfo{all:[sys_exps], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_exps], opt:0, req:[sys_exps], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_ansi_loop).

/*

### Compiled:  `SYS::ANSI-LOOP` 
*/
f_sys_ansi_loop(Exps_In, FnResult) :-
	Env10=[bv(sys_exps, Exps_In)|Env],
	global_env(Env),
	locally_set(ext_xx_warn_on_redefinition_xx,
		    [],
		    cl_require(loop, [], Require_Ret)),
	cl_fmakunbound(sys_ansi_loop, Fmakunbound_Ret),
	get_var(Env10, sys_exps, Exps_Get),
	[loop|Exps_Get]=FnResult.
:- set_opv(f_sys_ansi_loop, classof, claz_function),
   set_opv(sys_ansi_loop, compile_as, kw_function),
   set_opv(sys_ansi_loop, function, f_sys_ansi_loop),
   DefunResult=sys_ansi_loop.
/*
:- side_effect(assert_lsp(sys_ansi_loop,
			  lambda_def(defun,
				     sys_ansi_loop,
				     f_sys_ansi_loop,
				     [sys_exps],
				     
				     [ 
				       [ let,
					 [[ext_xx_warn_on_redefinition_xx, []]],
					 [require, [quote, loop]]
				       ],
				       [fmakunbound, [quote, sys_ansi_loop]],
				       
				       [ '#BQ',
					 [loop, ['#BQ-COMMA-ELIPSE', sys_exps]]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_ansi_loop,
			  arglist_info(sys_ansi_loop,
				       f_sys_ansi_loop,
				       [sys_exps],
				       arginfo{ all:[sys_exps],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_exps],
						opt:0,
						req:[sys_exps],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_ansi_loop, init_args(exact_only, f_sys_ansi_loop))).
*/
/*
(defmacro loop (&rest exps)
  (dolist (exp exps)
    (when (atom exp)
      (return-from loop (ansi-loop exps))))
  (let ((tag (gensym)))
    `(block nil (tagbody ,tag ,@exps (go ,tag)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12053 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,loop,['&rest',exps],[dolist,[exp,exps],[when,[atom,exp],['return-from',loop,['ansi-loop',exps]]]],[let,[[tag,[gensym]]],['#BQ',[block,[],[tagbody,['#COMMA',tag],['#BQ-COMMA-ELIPSE',exps],[go,['#COMMA',tag]]]]]]])
wl:lambda_def(defmacro, loop, cl_loop, [c38_rest, sys_exps], [progn, [dolist, [exp, sys_exps], [when, [atom, exp], [return_from, loop, [sys_ansi_loop, sys_exps]]]], [let, [[sys_tag, [gensym]]], ['#BQ', [block, [], [tagbody, ['#COMMA', sys_tag], ['#BQ-COMMA-ELIPSE', sys_exps], [go, ['#COMMA', sys_tag]]]]]]]).
wl:arglist_info(loop, cl_loop, [c38_rest, sys_exps], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_exps], opt:0, req:0, rest:[sys_exps], sublists:0, whole:0}).
wl: init_args(0, cl_loop).

/*

### Compiled:  `CL:LOOP` 
*/
cl_loop(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_exps, RestNKeys)|CDR],
	global_env(CDR),
	catch(( ( get_var(Env, sys_exps, Exps_Get),
		  BV=bv(exp, Ele),
		  BlockExitEnv=[BV|Env],
		  forall(member(Ele, Exps_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(BlockExitEnv, exp, Exp_Get),
			   (   Exp_Get\=[CAR|CDR33]
			   ->  get_var(BlockExitEnv, sys_exps, Exps_Get15),
			       f_sys_ansi_loop(Exps_Get15, RetResult),
			       throw(block_exit(loop, RetResult)),
			       _65904364=ThrowResult
			   ;   _65904364=[]
			   )
			 )),
		  cl_gensym(Tag_Init),
		  LEnv=[bv(sys_tag, Tag_Init)|Env],
		  get_var(LEnv, sys_exps, Exps_Get27),
		  get_var(LEnv, sys_tag, Tag_Get28),
		  bq_append([Tag_Get28|Exps_Get27],
			    [[go, Tag_Get28]],
			    Bq_append_Ret)
		),
		[block, [], [tagbody|Bq_append_Ret]]=MFResult
	      ),
	      block_exit(loop, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_loop, classof, claz_macro),
   set_opv(loop, compile_as, kw_operator),
   set_opv(loop, function, cl_loop),
   DefMacroResult=loop.
/*
:- side_effect(assert_lsp(loop,
			  lambda_def(defmacro,
				     loop,
				     cl_loop,
				     [c38_rest, sys_exps],
				     
				     [ progn,
				       
				       [ dolist,
					 [exp, sys_exps],
					 
					 [ when,
					   [atom, exp],
					   
					   [ return_from,
					     loop,
					     [sys_ansi_loop, sys_exps]
					   ]
					 ]
				       ],
				       
				       [ let,
					 [[sys_tag, [gensym]]],
					 
					 [ '#BQ',
					   
					   [ block,
					     [],
					     
					     [ tagbody,
					       ['#COMMA', sys_tag],
					       ['#BQ-COMMA-ELIPSE', sys_exps],
					       [go, ['#COMMA', sys_tag]]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(loop,
			  arglist_info(loop,
				       cl_loop,
				       [c38_rest, sys_exps],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_exps],
						opt:0,
						req:0,
						rest:[sys_exps],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(loop, init_args(0, cl_loop))).
*/
/*
(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  `(progn
     (sys::record-source-information-for-type ',var :variable)
     (%defvar ',var)
     ,@(when valp
         `((unless (boundp ',var)
             (setq ,var ,val))))
     ,@(when docp
         `((%set-documentation ',var 'variable ',doc)))
     ',var))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12243 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defvar,[var,'&optional',[val,[],valp],[doc,[],docp]],['#BQ',[progn,['sys::record-source-information-for-type',[quote,['#COMMA',var]],':variable'],['%defvar',[quote,['#COMMA',var]]],['#BQ-COMMA-ELIPSE',[when,valp,['#BQ',[[unless,[boundp,[quote,['#COMMA',var]]],[setq,['#COMMA',var],['#COMMA',val]]]]]]],['#BQ-COMMA-ELIPSE',[when,docp,['#BQ',[['%set-documentation',[quote,['#COMMA',var]],[quote,variable],[quote,['#COMMA',doc]]]]]]],[quote,['#COMMA',var]]]]])
wl:lambda_def(defmacro, defvar, cl_defvar, [sys_var, c38_optional, [sys_val, [], sys_valp], [sys_doc, [], sys_docp]], [progn, ['#BQ', [progn, [sys_record_source_information_for_type, [quote, ['#COMMA', sys_var]], kw_variable], [sys_pf_defvar, [quote, ['#COMMA', sys_var]]], ['#BQ-COMMA-ELIPSE', [when, sys_valp, ['#BQ', [[unless, [boundp, [quote, ['#COMMA', sys_var]]], [setq, ['#COMMA', sys_var], ['#COMMA', sys_val]]]]]]], ['#BQ-COMMA-ELIPSE', [when, sys_docp, ['#BQ', [[sys_pf_set_documentation, [quote, ['#COMMA', sys_var]], [quote, variable], [quote, ['#COMMA', sys_doc]]]]]]], [quote, ['#COMMA', sys_var]]]]]).
wl:arglist_info(defvar, cl_defvar, [sys_var, c38_optional, [sys_val, [], sys_valp], [sys_doc, [], sys_docp]], arginfo{all:[sys_var, sys_val, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_var, sys_val, sys_valp, sys_doc, sys_docp], opt:[sys_val, sys_doc], req:[sys_var], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_defvar).

/*

### Compiled:  `CL:DEFVAR` 
*/
cl_defvar(Var_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_var, Var_In), bv(sys_val, Val_In), bv(sys_valp, Valp_In), bv(sys_doc, Doc_In), bv(sys_docp, Docp_In)|Is_present_Param],
	global_env(Is_present_Param),
	arg_is_present(Is_present_Param, RestNKeys, sys_valp, Valp_In, 2),
	opt_var(Is_present_Param, sys_val, Val_In, true, [], 2, RestNKeys),
	arg_is_present(Is_present_Param, RestNKeys, sys_docp, Docp_In, 2),
	opt_var(Is_present_Param, sys_doc, Doc_In, true, [], 2, RestNKeys),
	get_var(Env, sys_valp, IFTEST),
	get_var(Env, sys_var, Var_Get14),
	(   IFTEST\==[]
	->  get_var(Env, sys_val, Val_Get),
	    get_var(Env, sys_var, Var_Get18),
	    CDR=[[unless, [boundp, [quote, Var_Get18]], [setq, Var_Get18, Val_Get]]]
	;   CDR=[]
	),
	get_var(Env, sys_docp, IFTEST21),
	(   IFTEST21\==[]
	->  get_var(Env, sys_doc, Doc_Get),
	    get_var(Env, sys_var, Var_Get24),
	    Bq_append_Param=[[sys_pf_set_documentation, [quote, Var_Get24], [quote, variable], [quote, Doc_Get]]]
	;   Bq_append_Param=[]
	),
	get_var(Env, sys_var, Var_Get26),
	bq_append(Bq_append_Param, [[quote, Var_Get26]], Bq_append_Ret),
	bq_append([[sys_pf_defvar, [quote, Var_Get14]]|CDR],
		  Bq_append_Ret,
		  Bq_append_Ret32),
	[progn, [sys_record_source_information_for_type, [quote, Var_Get14], kw_variable]|Bq_append_Ret32]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defvar, classof, claz_macro),
   set_opv(defvar, compile_as, kw_operator),
   set_opv(defvar, function, cl_defvar),
   DefMacroResult=defvar.
/*
:- side_effect(assert_lsp(defvar,
			  lambda_def(defmacro,
				     defvar,
				     cl_defvar,
				     
				     [ sys_var,
				       c38_optional,
				       [sys_val, [], sys_valp],
				       [sys_doc, [], sys_docp]
				     ],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ progn,
					   
					   [ sys_record_source_information_for_type,
					     [quote, ['#COMMA', sys_var]],
					     kw_variable
					   ],
					   
					   [ sys_pf_defvar,
					     [quote, ['#COMMA', sys_var]]
					   ],
					   
					   [ '#BQ-COMMA-ELIPSE',
					     
					     [ when,
					       sys_valp,
					       
					       [ '#BQ',
						 
						 [ 
						   [ unless,
						     
						     [ boundp,
						       
						       [ quote,
							 ['#COMMA', sys_var]
						       ]
						     ],
						     
						     [ setq,
						       ['#COMMA', sys_var],
						       ['#COMMA', sys_val]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ],
					   
					   [ '#BQ-COMMA-ELIPSE',
					     
					     [ when,
					       sys_docp,
					       
					       [ '#BQ',
						 
						 [ 
						   [ sys_pf_set_documentation,
						     
						     [ quote,
						       ['#COMMA', sys_var]
						     ],
						     [quote, variable],
						     
						     [ quote,
						       ['#COMMA', sys_doc]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ],
					   [quote, ['#COMMA', sys_var]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defvar,
			  arglist_info(defvar,
				       cl_defvar,
				       
				       [ sys_var,
					 c38_optional,
					 [sys_val, [], sys_valp],
					 [sys_doc, [], sys_docp]
				       ],
				       arginfo{ all:[sys_var, sys_val, sys_doc],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_var,
							sys_val,
							sys_valp,
							sys_doc,
							sys_docp
						      ],
						opt:[sys_val, sys_doc],
						req:[sys_var],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defvar, init_args(1, cl_defvar))).
*/
/*
(defmacro defconst (name value)
  `(defconstant ,name
     (if (boundp ',name)
         (symbol-value ',name)
         ,value)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12573 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defconst,[name,value],['#BQ',[defconstant,['#COMMA',name],[if,[boundp,[quote,['#COMMA',name]]],['symbol-value',[quote,['#COMMA',name]]],['#COMMA',value]]]]])
wl:lambda_def(defmacro, sys_defconst, f_sys_defconst, [sys_name, sys_value], [progn, ['#BQ', [defconstant, ['#COMMA', sys_name], [if, [boundp, [quote, ['#COMMA', sys_name]]], [symbol_value, [quote, ['#COMMA', sys_name]]], ['#COMMA', sys_value]]]]]).
wl:arglist_info(sys_defconst, f_sys_defconst, [sys_name, sys_value], arginfo{all:[sys_name, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_value], opt:0, req:[sys_name, sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_defconst).

/*

### Compiled:  `SYS:DEFCONST` 
*/
f_sys_defconst(Name_In, Value_In, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_name, Name_In), bv(sys_value, Value_In)|CDR],
	global_env(CDR),
	get_var(Env, sys_name, Name_Get9),
	get_var(Env, sys_value, Value_Get),
	[defconstant, Name_Get9, [if, [boundp, [quote, Name_Get9]], [symbol_value, [quote, Name_Get9]], Value_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_defconst, classof, claz_macro),
   set_opv(sys_defconst, compile_as, kw_operator),
   set_opv(sys_defconst, function, f_sys_defconst),
   DefMacroResult=sys_defconst.
/*
:- side_effect(assert_lsp(sys_defconst,
			  lambda_def(defmacro,
				     sys_defconst,
				     f_sys_defconst,
				     [sys_name, sys_value],
				     
				     [ progn,
				       
				       [ '#BQ',
					 
					 [ defconstant,
					   ['#COMMA', sys_name],
					   
					   [ if,
					     
					     [ boundp,
					       [quote, ['#COMMA', sys_name]]
					     ],
					     
					     [ symbol_value,
					       [quote, ['#COMMA', sys_name]]
					     ],
					     ['#COMMA', sys_value]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_defconst,
			  arglist_info(sys_defconst,
				       f_sys_defconst,
				       [sys_name, sys_value],
				       arginfo{ all:[sys_name, sys_value],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_name, sys_value],
						opt:0,
						req:[sys_name, sys_value],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_defconst, init_args(exact_only, f_sys_defconst))).
*/
/*
(load "wam-cl-init-1")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12704 **********************/
:-lisp_compile_to_prolog(pkg_sys,[load,'$STRING'("wam-cl-init-1")])
:- cl_load('$ARRAY'([*], claz_base_character, "wam-cl-init-1"), [], _Ignored).
/*
'(load "wam-cl-init2")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12727 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init2")]])
/*
'(load "wam-cl-init3")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12750 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init3")]])
/*
'(write-line " WAM CommonLisp ")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12773 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['write-line','$STRING'(" WAM CommonLisp ")]])
/*
'(read-eval-print-loop)

 

#|

;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "'(read-eval-print-loop)\n\n \n\n#|\n\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:12806 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['read-eval-print-loop']])
/*
; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) ".
*/
/*


;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "\n\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \n\n(defun packagesyminfo (p0)\n (let ((p (find-package p0)))\n (do-all-symbols (sym)    \n  (when (eq (symbol-package sym) p)\n   (format t \"symbolinfo('~s','~s').~%\"\n    sn (package-name (symbol-package sym))\n    (constantp sym)\n    (special-operator-p sym)\n    (symbol-plist sym)\n    sn (symbol-package sym)\n    (if (boundp sym) (symbol-value sym))\n    (if (fboundp sym) (type-of (symbol-function sym)))\n    (fboundp sym)))))))\n(packagesyminfo :cl)\n\n\n\n\n\n(defun packagesyminfo (p0)\n (let ((p (find-package p0)))\n (do-all-symbols (sym)    \n  (when (eq (symbol-package sym) p)\n   (format t \"symbol_package('~a','~a').~%\"\n    sn (package-name (symbol-package sym)))))))\n(packagesyminfo :cl)\n\n\n".
*/


%; Total compilation time: 20.859 seconds


#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_get" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:18 2017

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
 Load GATE.
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *question-mark-atom* '?)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:319 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*question-mark-atom*',[quote,?]])
:- set_var(AEnv, setq, u_xx_question_mark_atom_xx, ?).
/*
(progn
 (setq *gate-version* "GATE 2.3, Common Lisp version of 2004-12-20")
 (format t "======================="(progn\n (setq *gate-version* \"GATE 2.3, Common Lisp version of 2004-12-20\")\n (format t \"=======================~%\")\n (format t \"Loading ~A...~%\" *gate-version*)\n (format t \"=======================~%\")\n nil)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:352 **********************/
:-lisp_compile_to_prolog(pkg_user,[progn,[setq,'*gate-version*','$STRING'("GATE 2.3, Common Lisp version of 2004-12-20")],[format,t,'$STRING'("=======================~%")],[format,t,'$STRING'("Loading ~A...~%"),'*gate-version*'],[format,t,'$STRING'("=======================~%")],[]])
:- set_var(AEnv,
	   setq,
	   u_xx_gate_version_xx,
	   '$ARRAY'([*],
		    claz_base_character,
		    "GATE 2.3, Common Lisp version of 2004-12-20")),
   cl_format([t, '$ARRAY'([*], claz_base_character, "=======================~%")],
	     Format_Ret),
   get_var(AEnv, u_xx_gate_version_xx, Xx_gate_version_xx_Get),
   cl_format(
	     [ t,
	       '$ARRAY'([*], claz_base_character, "Loading ~A...~%"),
	       Xx_gate_version_xx_Get
	     ],
	     Format_Ret7),
   cl_format([t, '$ARRAY'([*], claz_base_character, "=======================~%")],
	     Format_Ret8).
/*
(load "compat")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:560 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("compat")])
:- cl_load('$ARRAY'([*], claz_base_character, "compat"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x35892f0)))).
*/
/*
(setq *gate-input* *standard-input*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:577 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*gate-input*','*standard-input*'])
:- get_var(AEnv, xx_standard_input_xx, Xx_standard_input_xx_Get),
   set_var(AEnv, u_xx_gate_input_xx, Xx_standard_input_xx_Get).
/*
(setq *gate-output* *standard-output*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:614 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*gate-output*','*standard-output*'])
:- get_var(AEnv, xx_standard_output_xx, Xx_standard_output_xx_Get),
   set_var(AEnv, u_xx_gate_output_xx, Xx_standard_output_xx_Get).
/*
(setq *gate-dbg* *standard-output*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:653 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*gate-dbg*','*standard-output*'])
:- get_var(AEnv, xx_standard_output_xx, Xx_standard_output_xx_Get),
   set_var(AEnv, u_xx_gate_dbg_xx, Xx_standard_output_xx_Get).
/*
(setq *gate-warn-dbg* t)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:689 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*gate-warn-dbg*',t])
:- set_var(AEnv, setq, u_xx_gate_warn_dbg_xx, t).
/*
(setq *gen-stream* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:714 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*gen-stream*',[]])
:- set_var(AEnv, setq, u_xx_gen_stream_xx, []).
/*
(if (not (boundp '*gate-load-options*))
    (setq *gate-load-options* nil)
    nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:739 **********************/
:-lisp_compile_to_prolog(pkg_user,[if,[not,[boundp,[quote,'*gate-load-options*']]],[setq,'*gate-load-options*',[]],[]])
:- cl_boundp(u_xx_gate_load_options_xx, PredArgResult),
   (   PredArgResult==[]
   ->  set_var(AEnv, setq, u_xx_gate_load_options_xx, []),
       _Ignored=[]
   ;   _Ignored=[]
   ).
/*
(load "loop")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:824 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("loop")])
:- cl_load('$ARRAY'([*], claz_base_character, "loop"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/loop.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x358b3d0)))).
*/
/*
(load "gate_macros")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:838 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_macros")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_macros"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x3654150)))).
*/
/*
(load "gate_main")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:860 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_main")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_main"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x9393f0)))).
*/
/*
(load "gate_ty")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:879 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_ty")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_ty"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x35ce280)))).
*/
/*
(load "gate_cx")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:896 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_cx")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_cx"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x37f9f90)))).
*/
/*
(load "gate_instan")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:913 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_instan")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_instan"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x38a8ff0)))).
*/
/*
(load "gate_prove")
; (load "gate_read_pr")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:934 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_prove")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_prove"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x3905060)))).
*/
/*
(load "gate_unify")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:978 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_unify")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_unify"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x3721070)))).
*/
/*
(load "gate_utils")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:998 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_utils")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_utils"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x3620840)))).
*/
/*
(load "gate_obs")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1019 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("gate_obs")])
:- cl_load('$ARRAY'([*], claz_base_character, "gate_obs"), [], _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_obs.cl'),
				  lisp_compile_to_prolog_output(<stream>(0x39868f0)))).
*/
/*
(interest 'ob-warn 'all)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1038 **********************/
:-lisp_compile_to_prolog(pkg_user,[interest,[quote,'ob-warn'],[quote,all]])
:- f_u_interest(u_ob_warn, [u_all], _Ignored).
/*
(interest 'context 'all)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1063 **********************/
:-lisp_compile_to_prolog(pkg_user,[interest,[quote,context],[quote,all]])
:- f_u_interest(u_context, [u_all], _Ignored).
/*
(format t "======================="(format t \"=======================~%\")\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1089 **********************/
:-lisp_compile_to_prolog(pkg_user,[format,t,'$STRING'("=======================~%")])
:- cl_format([t, '$ARRAY'([*], claz_base_character, "=======================~%")],
	     _Ignored).
/*
(format t "Welcome to "(format t \"Welcome to ~A~%\" *gate-version*)\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1128 **********************/
:-lisp_compile_to_prolog(pkg_user,[format,t,'$STRING'("Welcome to ~A~%"),'*gate-version*'])
:- get_var(GEnv, u_xx_gate_version_xx, Xx_gate_version_xx_Get),
   cl_format(
	     [ t,
	       '$ARRAY'([*], claz_base_character, "Welcome to ~A~%"),
	       Xx_gate_version_xx_Get
	     ],
	     _Ignored).
/*
(format t "======================="(format t \"=======================~%\")\n    \n; ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_get.cl:1172 **********************/
:-lisp_compile_to_prolog(pkg_user,[format,t,'$STRING'("=======================~%")])
:- cl_format([t, '$ARRAY'([*], claz_base_character, "=======================~%")],
	     _Ignored).
/*
 End of file.
*/


%; Total compilation time: 74.066 seconds


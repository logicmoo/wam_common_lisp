#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "../prolog/wam_cl/wam-cl-init" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Fri Dec 22 04:05:57 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(in-package #:system)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:system'])
:- cl_in_package(system1, _Ignored4).
/*
(defpackage "SYSTEM" (:nicknames "SYS"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:27 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("SYSTEM"),[':nicknames','$STRING'("SYS")]])
:- cl_defpackage('$ARRAY'([*], claz_base_character, "SYSTEM"),
		 [[kw_nicknames, '$ARRAY'([*], claz_base_character, "SYS")]],
		 _Ignored4).
/*
:- side_effect(add_opv_new(pkg_system, nicknames, "SYS")).
*/
/*
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:69 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("COMMON-LISP"),[':nicknames','$STRING'("CL"),'$STRING'("LISP")],[':uses','$STRING'("SYSTEM")]])
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "CL"),
		     '$ARRAY'([*], claz_base_character, "LISP")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "SYSTEM")]
		 ],
		 _Ignored4).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:138 **********************/
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
		 _Ignored4).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:227 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*lisp-file-type*','$STRING'("lisp")])
:- set_var(TLEnv5,
	   defvar,
	   sys_xx_lisp_file_type_xx,
	   '$ARRAY'([*], claz_base_character, "lisp")).
/*
(defvar *default-pathname-defaults* #P"")
 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:262 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*default-pathname-defaults*','$OBJ'(claz_pathname,"")])
:- set_var(TLEnv5,
	   defvar,
	   xx_default_pathname_defaults_xx,
	   '$OBJ'(claz_pathname, "")).
/*
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:308 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,dd,[],[let,[['*lisp-file-type*','$STRING'("cl")],['*default-pathname-defaults*',['merge-pathnames','$STRING'("daydreamer/")]]],[load,'$STRING'("dd")]]])
wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]).
wl:arglist_info(sys_dd, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_dd).

/*

### Compiled:  `SYS::DD` 
*/
f_sys_dd(FnResult) :-
	Env=[],
	cl_merge_pathnames('$ARRAY'([*], claz_base_character, "daydreamer/"),
			   Xx_default_pathname_defaults_xx_Init),
	LEnv=[[bv(sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl"))]|Env],
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
   _Ignored4=sys_dd.
/*
:- side_effect(assert_lsp(sys_dd,
			  wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]))).
*/
/*
:- side_effect(assert_lsp(sys_dd,
			  wl:arglist_info(sys_dd, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_dd, wl:init_args(exact_only, sys_dd))).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:446 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'show-ascii-art',[],['write-line','$STRING'("  __________    ")],['write-line','$STRING'(" / ___  ___ \\   ")],['write-line','$STRING'("/ / @ \\/ @ \\ \\  ")],['write-line','$STRING'("\\ \\___/\\___/ /\\ ")],['write-line','$STRING'(" \\____\\/____/|| ")],['write-line','$STRING'(" /     /\\\\\\\\\\// ")],['write-line','$STRING'("|     |\\\\\\\\\\\\   ")],['write-line','$STRING'(" \\      \\\\\\\\\\\\  ")],['write-line','$STRING'("   \\______/\\\\\\\\ ")],['write-line','$STRING'("    _||_||_     ")],['write-line','$STRING'("                ")]])
wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]).
wl:arglist_info(sys_show_ascii_art, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_show_ascii_art).

/*

### Compiled:  `SYS::SHOW-ASCII-ART` 
*/
f_sys_show_ascii_art(FnResult) :-
	Env=[],
	cl_write_line('$ARRAY'([*], claz_base_character, "  __________    "),
		      Write_line_Ret),
	cl_write_line('$ARRAY'([*], claz_base_character, " / ___  ___ \\   "),
		      Write_line_Ret15),
	cl_write_line('$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  "),
		      Write_line_Ret16),
	cl_write_line('$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ "),
		      Write_line_Ret17),
	cl_write_line('$ARRAY'([*], claz_base_character, " \\____\\/____/|| "),
		      Write_line_Ret18),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " /     /\\\\\\\\\\// "),
		      Write_line_Ret19),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "|     |\\\\\\\\\\\\   "),
		      Write_line_Ret20),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " \\      \\\\\\\\\\\\  "),
		      Write_line_Ret21),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "   \\______/\\\\\\\\ "),
		      Write_line_Ret22),
	cl_write_line('$ARRAY'([*], claz_base_character, "    _||_||_     "),
		      Write_line_Ret23),
	cl_write_line('$ARRAY'([*], claz_base_character, "                "),
		      Write_line_Ret24),
	Write_line_Ret24=FnResult.
:- set_opv(f_sys_show_ascii_art, classof, claz_function),
   set_opv(sys_show_ascii_art, compile_as, kw_function),
   set_opv(sys_show_ascii_art, function, f_sys_show_ascii_art),
   _Ignored4=sys_show_ascii_art.
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:arglist_info(sys_show_ascii_art, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:init_args(exact_only, sys_show_ascii_art))).
*/
/*
(show-ascii-art)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:881 **********************/
:-lisp_compile_to_prolog(pkg_sys,['show-ascii-art'])
:- f_sys_show_ascii_art(_Ignored4).
/*
(load "wam-cl-init-1")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:899 **********************/
:-lisp_compile_to_prolog(pkg_sys,[load,'$STRING'("wam-cl-init-1")])
:- cl_load('$ARRAY'([*], claz_base_character, "wam-cl-init-1"), [], _Ignored4).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp'),
				  lisp_compile_to_prolog_output(<stream>(0x1d4a860)))).
*/
/*
'(load "wam-cl-init2")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:923 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init2")]])
/*
'(load "wam-cl-init3")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:947 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init3")]])
/*
'(write-line " WAM CommonLisp ")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:971 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['write-line','$STRING'(" WAM CommonLisp ")]])
/*
'(read-eval-print-loop)

 

#|

;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "'(read-eval-print-loop)\r\n\r\n \r\n\r\n#|\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1005 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['read-eval-print-loop']])
/*
; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) ".
*/
/*


;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbolinfo('~s','~s').~%\"\r\n    sn (package-name (symbol-package sym))\r\n    (constantp sym)\r\n    (special-operator-p sym)\r\n    (symbol-plist sym)\r\n    sn (symbol-package sym)\r\n    (if (boundp sym) (symbol-value sym))\r\n    (if (fboundp sym) (type-of (symbol-function sym)))\r\n    (fboundp sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n\r\n\r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbol_package('~a','~a').~%\"\r\n    sn (package-name (symbol-package sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n".
*/


%; Total compilation time: 6.822 seconds

